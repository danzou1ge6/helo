use crate::ast;
use crate::ast::Trie;
use crate::errors;
use crate::parse::tast;
use errors::ManyError;

use tast::NameSpace;

pub type OpPriority = u32;

#[derive(Clone, Copy)]
pub struct Precedence(pub OpPriority, pub OpPriority);

impl Precedence {
    pub fn left(&self) -> OpPriority {
        self.0
    }
    pub fn right(&self) -> OpPriority {
        self.1
    }
}

#[derive(Clone)]
pub struct PrecedenceTable<'s>(imbl::HashMap<&'s str, Precedence>);

impl<'s> PrecedenceTable<'s> {
    pub fn new() -> Self {
        Self(
            [
                (".", Precedence(1000, 1001)),
                ("and", Precedence(40, 41)),
                ("or", Precedence(40, 41)),
                ("+", Precedence(40, 41)),
                ("-", Precedence(40, 41)),
                ("*", Precedence(50, 51)),
                ("/", Precedence(50, 51)),
                ("**", Precedence(61, 60)),
                ("mod", Precedence(41, 40)),
                ("==", Precedence(31, 30)),
                ("/=", Precedence(31, 30)),
                (">=", Precedence(31, 30)),
                ("<=", Precedence(31, 30)),
                (">", Precedence(31, 30)),
                ("<", Precedence(31, 30)),
                ("<apply>", Precedence(0, 0)),
            ]
            .into_iter()
            .collect(),
        )
    }

    pub fn insert(&mut self, k: &'s str, v: Precedence) -> Option<Precedence> {
        self.0.insert(k, v)
    }

    pub fn get(&self, k: &str) -> Option<&Precedence> {
        self.0.get(k)
    }

    pub const DEFAULT: u32 = 0;

    pub fn priority_left(&self, id: &str) -> OpPriority {
        self.get(id)
            .map(|p| p.left())
            .map_or_else(|| Self::DEFAULT, |x| x)
    }
    pub fn priority_right(&self, id: &str) -> OpPriority {
        self.get(id)
            .map(|p| p.right())
            .map_or_else(|| Self::DEFAULT, |x| x)
    }
}

/// .2 indicates mutable
type ResolutionEntry<'s> = (&'s str, ast::LocalId, bool, ast::Meta);

#[derive(Default)]
pub struct FunctionResolutionEnv<'s> {
    locals: Vec<ResolutionEntry<'s>>,
    scope_cnt: Vec<usize>,
    id_cnt: ast::LocalId,
}

pub struct ClosureResolutionEnv<'s> {
    env: FunctionResolutionEnv<'s>,
    captures: Vec<(ast::Capture, bool)>,
    this_name: &'s str,
    /// Indicates if this closure or its child closures capture this closure.
    /// Type inference become tricky under such circumstance, so we need this information
    /// to force type annotation.
    recursive: Option<ast::Meta>,
}

pub struct ClosureInfo {
    pub captures: Vec<ast::Capture>,
    pub local_cnt: usize,
    pub recursive: Option<ast::Meta>,
}

impl<'s> From<ClosureResolutionEnv<'s>> for ClosureInfo {
    fn from(value: ClosureResolutionEnv<'s>) -> Self {
        Self {
            captures: value.captures.into_iter().map(|(c, _)| c).collect(),
            local_cnt: value.env.local_cnt(),
            recursive: value.recursive,
        }
    }
}

impl<'s> ClosureResolutionEnv<'s> {
    pub fn capture(&mut self, cap: ast::Capture, mutable: bool) -> ast::CapturedId {
        if let Some((i, _)) = self
            .captures
            .iter()
            .enumerate()
            .find(|(_, (x, _))| *x == cap)
        {
            return ast::CapturedId(i);
        }
        let id = ast::CapturedId(self.captures.len());
        self.captures.push((cap, mutable));
        id
    }
    pub fn new(name: &'s str) -> Self {
        Self {
            env: FunctionResolutionEnv::new(),
            captures: Vec::new(),
            this_name: name,
            recursive: None,
        }
    }
}

impl<'s> FunctionResolutionEnv<'s> {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            scope_cnt: vec![0],
            id_cnt: 0.into(),
        }
    }
    fn local_cnt(&self) -> usize {
        self.id_cnt.into()
    }
    pub fn define_local(&mut self, name: &'s str, mutable: bool, meta: ast::Meta) -> ast::LocalId {
        let id = self.id_cnt;
        self.locals.push((name, id, mutable, meta));
        *self.scope_cnt.last_mut().unwrap() += 1;
        self.id_cnt = id + 1;
        id
    }
    pub fn lookup_local(&self, name: &'s str) -> Option<ResolutionEntry<'s>> {
        self.locals
            .iter()
            .rev()
            .find(|(sid, _, _, _)| *sid == name)
            .cloned()
    }
}

pub struct ResolutionEnv<'s> {
    base: FunctionResolutionEnv<'s>,
    closures: Vec<ClosureResolutionEnv<'s>>,
}

impl<'s> ResolutionEnv<'s> {
    fn new() -> Self {
        Self {
            base: FunctionResolutionEnv::new(),
            closures: Vec::new(),
        }
    }
    fn enter_closure(&mut self, name: &'s str) {
        self.closures.push(ClosureResolutionEnv::new(name))
    }
    fn exit_closure(&mut self) -> ClosureInfo {
        self.closures.pop().unwrap().into()
    }
    fn current(&mut self) -> &mut FunctionResolutionEnv<'s> {
        match self.closures.last_mut() {
            Some(c) => &mut c.env,
            None => &mut self.base,
        }
    }
    fn pop(&mut self, cnt: usize) {
        let new_len = self.current().locals.len() - cnt;
        self.current()
            .locals
            .resize_with(new_len, || unreachable!());
    }
    fn pop_scope(&mut self) {
        let pop_cnt = self.current().scope_cnt.pop().unwrap();
        self.pop(pop_cnt);
    }
}

pub struct GlobalSymbols<'s, 'sy, B, F, C, D, R> {
    pub builtins: &'sy Trie<ast::BuiltinFunctionName<'s>, B, &'s str>,
    pub functions: &'sy Trie<ast::FunctionName<'s>, F, &'s str>,
    pub constructors: &'sy Trie<ast::ConstructorName<'s>, C, &'s str>,
    pub datas: &'sy Trie<ast::DataName<'s>, D, &'s str>,
    pub relations: &'sy Trie<ast::RelationName<'s>, R, &'s str>,
    pub modules: &'sy Trie<ast::Path<'s>, tast::NameSpace<'s>, &'s str>,
    pub methods: &'sy Trie<ast::FunctionName<'s>, ast::RelationName<'s>, &'s str>,
}

impl<'s, 'sy, B, F, C, D, R> GlobalSymbols<'s, 'sy, B, F, C, D, R> {
    pub fn contains(&self, p: ast::PathRefIter<'_, 's>) -> bool {
        self.builtins.contains_path(p)
            || self.constructors.contains_path(p)
            || self.datas.contains_path(p)
            || self.functions.contains_path(p)
            || self.relations.contains_path(p)
            || self.methods.contains_path(p)
            || self.modules.contains_path(p)
    }
}

pub struct Resolver<'s, 'sy, B, F, C, D, R> {
    local: ResolutionEnv<'s>,
    pub global: NameSpace<'s>,
    pub symbols: GlobalSymbols<'s, 'sy, B, F, C, D, R>,
}

impl<'s, 'sy, B, F, C, D, R> Resolver<'s, 'sy, B, F, C, D, R> {
    /// Returns never if resolve fails
    pub fn resolve_global(
        &self,
        path: tast::Path<'s>,
        e: &mut ManyError,
        meta: ast::Meta,
    ) -> ast::Expr<'s> {
        use tast::IdentifierResolveError::*;
        let mut found = false;
        let mut paths = Vec::new();
        let r = match self
            .global
            .resolve_to_name(path.clone(), self.symbols.builtins)
        {
            Ok(bn) => {
                found = true;
                paths.push(ast::Path::from(bn.clone()));
                ast::Expr::new_untyped(ast::ExprNode::Builtin(bn), meta.clone())
            }
            Err(NoHit) => ast::Expr::new_never(meta.clone()),
            Err(other) => {
                other.commit(&path, e);
                return ast::Expr::new_never(meta);
            }
        };
        let r = match self
            .global
            .resolve_to_name(path.clone(), self.symbols.functions)
        {
            Ok(f_name) if found == false => {
                found = true;
                paths.push(ast::Path::from(f_name.clone()));
                ast::Expr::new_untyped(ast::ExprNode::UserFunction(f_name), meta.clone())
            }
            Ok(f_name) => {
                paths.push(f_name.into());
                Ambiguity(paths).commit(&path, e);
                return ast::Expr::new_never(meta);
            }
            Err(NoHit) => r,
            Err(other) => {
                other.commit(&path, e);
                return ast::Expr::new_never(meta);
            }
        };
        let r = match self
            .global
            .resolve_to_name(path.clone(), self.symbols.methods)
        {
            Ok(f_name) if found == false => {
                found = true;
                paths.push(ast::Path::from(f_name.clone()));
                ast::Expr::new_untyped(ast::ExprNode::UserFunction(f_name), meta.clone())
            }
            Ok(f_name) => {
                paths.push(f_name.into());
                Ambiguity(paths).commit(&path, e);
                return ast::Expr::new_never(meta);
            }
            Err(NoHit) => r,
            Err(other) => {
                other.commit(&path, e);
                return ast::Expr::new_never(meta);
            }
        };
        let r = match self
            .global
            .resolve_to_name(path.clone(), self.symbols.constructors)
        {
            Ok(cn) if found == false => {
                ast::Expr::new_untyped(ast::ExprNode::Constructor(cn), meta)
            }
            Ok(cn) => {
                paths.push(cn.into());
                Ambiguity(paths).commit(&path, e);
                return ast::Expr::new_never(meta);
            }
            Err(NoHit) if found == true => r,
            Err(NoHit) => {
                NoHit.commit(&path, e);
                r
            }
            Err(other) => {
                other.commit(&path, e);
                return ast::Expr::new_never(meta);
            }
        };
        r
    }

    pub fn resolve(&mut self, name: &str, meta: &ast::Meta) -> Option<ast::Expr<'s>> {
        if let Some((_, id, mutable, _)) = self.local.current().lookup_local(name) {
            return Some(ast::Expr::new_untyped(
                ast::ExprNode::Local(id, mutable),
                meta.clone(),
            ));
        }

        if self.local.closures.len() == 0 {
            return None;
        }

        let (cap_id, mutable, _) =
            self.resolve_capture_at_level(self.local.closures.len() - 1, name)?;
        let is_self = self.local.closures.last().unwrap().this_name == name;
        Some(ast::Expr::new_untyped(
            ast::ExprNode::Captured {
                id: cap_id,
                is_self,
                mutable,
            },
            meta.clone(),
        ))
    }

    /// Look for identifier, starting at `level`. Level 0 is first enclosing closure.
    fn resolve_capture_at_level(
        &mut self,
        level: usize,
        name: &str,
    ) -> Option<(ast::CapturedId, bool, ast::Meta)> {
        if level == 0 {
            if let Some((_, id, m, meta)) = self.local.base.lookup_local(name) {
                if self.local.closures[0].this_name == name {
                    self.local.closures[0].recursive = Some(meta.clone());
                }
                return Some((
                    self.local.closures[0].capture(ast::Capture::Local(id, meta.clone()), m),
                    m,
                    meta,
                ));
            }
            None
        } else {
            if let Some((_, id, m, meta)) = self.local.closures[level - 1].env.lookup_local(name) {
                if self.local.closures[level].this_name == name {
                    self.local.closures[level].recursive = Some(meta.clone());
                }
                return Some((
                    self.local.closures[level].capture(ast::Capture::Local(id, meta.clone()), m),
                    m,
                    meta,
                ));
            }
            if let Some((cap, m, meta)) = self.resolve_capture_at_level(level - 1, name) {
                return Some((
                    self.local.closures[level].capture(ast::Capture::Capture(cap, meta.clone()), m),
                    m,
                    meta,
                ));
            }
            None
        }
    }
    pub fn define_local(&mut self, name: &'s str, mutable: bool, meta: ast::Meta) -> ast::LocalId {
        self.local.current().define_local(name, mutable, meta)
    }

    // Returns local count of the function, along with result of `f`
    pub fn with_function_scope<R1>(
        &mut self,
        f: impl FnOnce(&mut Resolver<'s, 'sy, B, F, C, D, R>) -> R1,
    ) -> (usize, R1) {
        self.local = ResolutionEnv::new();
        let r = f(self);
        (self.local.current().local_cnt(), r)
    }

    /// All variables defined in `f` will be popped upon exit
    pub fn with_scope<R1>(
        &mut self,
        f: impl FnOnce(&mut Resolver<'s, 'sy, B, F, C, D, R>) -> R1,
    ) -> R1 {
        self.local.current().scope_cnt.push(0);
        let r = f(self);

        self.local.pop_scope();
        r
    }

    /// Set current status to "in-closure", that is , identifiers are resolved further in surrunding namespace
    /// if they are not found in local namespace.
    /// Return (captures, number of locals, recursive, result of `f`)
    pub fn with_closure_scope<R1>(
        &mut self,
        this_name: &'s str,
        f: impl FnOnce(&mut Resolver<'s, 'sy, B, F, C, D, R>) -> R1,
    ) -> (ClosureInfo, R1) {
        self.local.enter_closure(this_name);

        let r = f(self);

        let info = self.local.exit_closure();

        (info, r)
    }
    pub fn new(ns: NameSpace<'s>, sy: GlobalSymbols<'s, 'sy, B, F, C, D, R>) -> Self {
        Self {
            local: ResolutionEnv::new(),
            global: ns,
            symbols: sy,
        }
    }
}
