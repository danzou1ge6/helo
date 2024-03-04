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
                ("and", Precedence(41, 40)),
                ("or", Precedence(41, 40)),
                ("+", Precedence(41, 40)),
                ("-", Precedence(41, 40)),
                ("*", Precedence(51, 50)),
                ("/", Precedence(51, 50)),
                ("**", Precedence(61, 60)),
                ("mod", Precedence(41, 40)),
                ("==", Precedence(31, 30)),
                ("/=", Precedence(31, 30)),
                (">=", Precedence(31, 30)),
                ("<=", Precedence(31, 30)),
                (">", Precedence(31, 30)),
                ("<", Precedence(31, 30)),
                ("<apply>", Precedence(0, 0))
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
        self.get(id).map(|p| p.left()).map_or_else(|| Self::DEFAULT , |x| x)
    }
    pub fn priority_right(&self, id: &str) -> OpPriority {
        self.get(id).map(|p| p.right()).map_or_else(|| Self::DEFAULT, |x| x)
    }
}

/// .2 indicates mutable
type ResolutionEntry<'s> = (&'s str, ast::LocalId, bool);

#[derive(Default)]
pub struct FunctionResolutionEnv<'s> {
    locals: Vec<ResolutionEntry<'s>>,
    scope_cnt: Vec<usize>,
    id_cnt: ast::LocalId,
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
}

pub enum ResolutionEnv<'s> {
    Normal(FunctionResolutionEnv<'s>),
    InClosure {
        parent: FunctionResolutionEnv<'s>,
        current: FunctionResolutionEnv<'s>,
        captures: Vec<ast::LocalId>,
        caputures_meta: Vec<ast::Meta>,
        this_name: &'s str,
    },
}

impl<'s> ResolutionEnv<'s> {
    fn current(&mut self) -> &mut FunctionResolutionEnv<'s> {
        match self {
            ResolutionEnv::Normal(current) => current,
            ResolutionEnv::InClosure { current, .. } => current,
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
    pub methods: &'sy Trie<ast::FunctionName<'s>, ast::RelationName<'s>, &'s str>
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
    pub symbols: GlobalSymbols<'s, 'sy, B, F, C, D, R>
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
        let r = match self.global.resolve_to_name(path.clone(), self.symbols.builtins) {
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
        match self {
            Self {
                local: ResolutionEnv::Normal(f_res),
                ..
            } => f_res
                .locals
                .iter()
                .rfind(|(n, _, _)| *n == name)
                .map(|(_, id, mutable)| {
                    ast::Expr::new_untyped(ast::ExprNode::Local(*id, *mutable), meta.clone())
                }),

            // See documentation on [`ast::Closure_`] for how locals are laid out
            Self {
                local:
                    ResolutionEnv::InClosure {
                        parent,
                        current,
                        captures,
                        caputures_meta,
                        this_name,
                    },
                ..
            } => {
                current
                    .locals
                    .iter()
                    .rfind(|(n, _, _)| *n == name)
                    .map_or_else(
                        // Identifier not found as local, look in parent namespace, that is the function body surrounding the closure
                        || {
                            parent.locals.iter().rfind(|(n, _, _)| *n == name).map_or(
                                None,
                                |(_, id, mutable)| {
                                    // Found in parent namespace
                                    if let Some(i) = captures.iter().position(|i| *i == *id) {
                                        Some(ast::Expr::new_untyped(
                                            ast::ExprNode::Captured {
                                                id: i.into(),
                                                is_self: *this_name == name,
                                                mutable: *mutable,
                                            },
                                            meta.clone(),
                                        ))
                                    } else {
                                        captures.push(*id);
                                        caputures_meta.push(meta.clone());
                                        Some(ast::Expr::new_untyped(
                                            ast::ExprNode::Captured {
                                                id: (captures.len() - 1).into(),
                                                is_self: *this_name == name,
                                                mutable: *mutable,
                                            },
                                            meta.clone(),
                                        ))
                                    }
                                },
                            )
                        },
                        // Local
                        |(_, id, mutable)| {
                            Some(ast::Expr::new_untyped(
                                ast::ExprNode::Local(*id, *mutable),
                                meta.clone(),
                            ))
                        },
                    )
            }
        }
    }
    pub fn define_local(&mut self, name: &'s str, mutable: bool) -> ast::LocalId {
        let id = self.local.current().id_cnt;
        self.local.current().locals.push((name, id, mutable));
        *self.local.current().scope_cnt.last_mut().unwrap() += 1;
        self.local.current().id_cnt = id + 1;
        id
    }

    // Returns local count of the function, along with result of `f`
    pub fn with_function_scope<R1>(
        &mut self,
        f: impl FnOnce(&mut Resolver<'s, 'sy, B, F, C, D, R>) -> R1,
    ) -> (usize, R1) {
        self.local = ResolutionEnv::Normal(FunctionResolutionEnv::new());
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
    /// Return (captures, number of locals, result of `f`)
    pub fn with_closure_scope<R1>(
        &mut self,
        this_name: &'s str,
        f: impl FnOnce(&mut Resolver<'s, 'sy, B, F, C, D, R>) -> R1,
    ) -> (Vec<ast::LocalId>, Vec<ast::Meta>, usize, R1) {
        let parent = match &mut self.local {
            ResolutionEnv::Normal(p) => std::mem::take(p),
            _ => unreachable!(),
        };
        self.local = ResolutionEnv::InClosure {
            parent,
            current: FunctionResolutionEnv::new(),
            captures: Vec::new(),
            caputures_meta: Vec::new(),
            this_name,
        };

        let r = f(self);

        let (parent, captures, captures_meta, locals_cnt) = match &mut self.local {
            ResolutionEnv::InClosure {
                parent,
                captures,
                current,
                caputures_meta,
                ..
            } => (
                std::mem::take(parent),
                std::mem::take(captures),
                std::mem::take(caputures_meta),
                current.local_cnt(),
            ),
            _ => unreachable!(),
        };
        self.local = ResolutionEnv::Normal(parent);
        (captures, captures_meta, locals_cnt, r)
    }
    pub fn new(
        ns: NameSpace<'s>,
        sy: GlobalSymbols<'s, 'sy, B, F, C, D, R>
    ) -> Self {
        Self {
            local: ResolutionEnv::Normal(FunctionResolutionEnv::new()),
            global: ns,
            symbols: sy
        }
    }
}
