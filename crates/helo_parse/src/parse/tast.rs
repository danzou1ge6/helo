use crate::ast::{self, ConstructTriePath};
use crate::errors;

#[derive(Debug, Clone, Hash)]
/// A user provided path that may be further resolve according to the context.
pub struct Path<'s> {
    pub path: Vec<&'s str>,
    pub meta: ast::Meta,
}

impl<'s> std::fmt::Display for Path<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ast::str_join_vec(f, ".", &self.path)
    }
}

impl<'s> Path<'s> {
    pub fn absolute(&self, at: &ast::Path<'s>) -> Result<ast::Path<'s>, ()> {
        let mut iter = self.path.iter();
        let (iter, mut mp) = if iter.next().is_some_and(|x| *x == "root") {
            (iter, Vec::new())
        } else {
            (self.path.iter(), at.0.clone())
        };

        for &item in iter {
            match item {
                "super" => mp.pop().map_or_else(|| Err(()), |_| Ok(()))?,
                _ => mp.push(item),
            }
        }

        Ok(ast::Path(mp))
    }

    pub fn no_super_root(&self) -> bool {
        self.path.iter().all(|x| *x != "super" && *x != "root")
    }
}

impl<'s> Path<'s> {
    pub fn len(&self) -> usize {
        self.path.len()
    }
    pub fn unwrap_first(&self) -> &'s str {
        self.path[0]
    }
    pub fn only_one(&self) -> Option<&'s str> {
        if self.len() == 1 {
            Some(self.path[0])
        } else {
            None
        }
    }
    pub fn skeleton(id: &'s str, meta: Meta) -> Self {
        Self {
            path: vec![id],
            meta,
        }
    }
    pub fn popped_first(self) -> Self {
        let mut path = self.path;
        path.remove(0);
        Self {
            path,
            meta: self.meta,
        }
    }
    pub fn pushed(mut self, id: &'s str) -> Self {
        self.path.push(id);
        self
    }
}

impl<'s> PartialEq for Path<'s> {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}
impl<'s> Eq for Path<'s> {}

#[derive(Clone, Debug)]
pub enum NameSpaceOp<'s> {
    Open(Path<'s>),
    Alias(&'s str, Path<'s>),
}

#[derive(Clone, Debug)]
/// Paths in this enum are assured to exist
pub enum NameSpaceOpSafe<'s> {
    Open(ast::Path<'s>),
    Alias(&'s str, ast::Path<'s>),
}

#[derive(Debug, Clone)]
pub struct NameSpace<'s> {
    pub opened: imbl::HashSet<ast::Path<'s>>,
    pub aliases: imbl::HashMap<&'s str, ast::Path<'s>>,
    pub current: ast::Path<'s>,
}

pub enum IdentifierResolveError<'s> {
    Ambiguity(Vec<ast::Path<'s>>),
    PathBeyondRoot,
    NoHit,
}

impl<'s> IdentifierResolveError<'s> {
    pub fn commit(self, p: &Path<'s>, e: &mut errors::ManyError) {
        match self {
            Self::Ambiguity(hits) => {
                e.push(errors::AmbiguityResolvingSymbol::new(&p.meta, &hits, p))
            }
            Self::NoHit => e.push(errors::IdentifierPathNotFound::new(p, &p.meta)),
            Self::PathBeyondRoot => e.push(errors::ModulePathBeyondRoot::new(&p.meta, p)),
        }
    }
}

impl<'s> NameSpace<'s> {
    pub fn new(current: ast::Path<'s>) -> Self {
        let mut opened = imbl::HashSet::new();
        opened.insert(current.clone());
        opened.insert(ast::Path::empty());
        Self {
            opened,
            aliases: imbl::HashMap::new(),
            current,
        }
    }

    pub fn resolve_op(
        &self,
        op: NameSpaceOp<'s>,
        exists: &impl Fn(&ast::Path<'s>) -> bool,
        e: &mut errors::ManyError,
    ) -> Option<NameSpaceOpSafe<'s>> {
        use NameSpaceOp::*;
        match op {
            Open(p) => Some(NameSpaceOpSafe::Open(
                self.resolve_module(p.clone(), exists)
                    .map_err(|err| {
                        err.commit(&p, e);
                    })
                    .ok()?,
            )),
            Alias(id, p) => Some(NameSpaceOpSafe::Alias(
                id,
                self.resolve_module(p.clone(), exists)
                    .map_err(|err| {
                        err.commit(&p, e);
                    })
                    .ok()?,
            )),
        }
    }

    /// No guarantee that the resulting path exists
    pub fn resolve(&self, p: Path<'s>) -> Result<Vec<ast::Path<'s>>, ()> {
        if let Some(mp) = self.aliases.get(p.path[0]) {
            let p1 = p.popped_first();
            let p = p1.absolute(mp)?;
            return Ok(vec![p]);
        }
        if p.no_super_root() {
            let hits: Vec<_> = self
                .opened
                .iter()
                .map(|mp| p.absolute(mp).unwrap())
                .collect();
            return Ok(hits);
        }
        let p = p.absolute(&self.current)?;
        Ok(vec![p])
    }

    pub fn resolve_module(
        &self,
        p: Path<'s>,
        exists: &impl Fn(&ast::Path<'s>) -> bool,
    ) -> Result<ast::Path<'s>, IdentifierResolveError<'s>> {
        let hits = self
            .resolve(p)
            .map_err(|_| IdentifierResolveError::PathBeyondRoot)?
            .into_iter()
            .filter(|p| exists(p))
            .collect::<Vec<_>>();
        if hits.len() == 0 {
            Err(IdentifierResolveError::NoHit)
        } else if hits.len() == 1 {
            let mut hits = hits;
            Ok(hits.pop().unwrap())
        } else {
            Err(IdentifierResolveError::Ambiguity(hits))
        }
    }

    pub fn apply_safe(&mut self, op: NameSpaceOpSafe<'s>) {
        use NameSpaceOpSafe::*;
        match op {
            Open(p) => {
                self.opened.insert(p);
            }
            Alias(id, p) => {
                self.aliases.insert(id, p);
            }
        };
    }

    pub fn apply(
        &mut self,
        op: NameSpaceOp<'s>,
        exists: &impl Fn(&ast::Path<'s>) -> bool,
        e: &mut errors::ManyError,
    ) {
        if let Some(op) = self.resolve_op(op, exists, e) {
            self.apply_safe(op)
        }
    }

    pub fn applied(
        mut self,
        op: NameSpaceOp<'s>,
        exists: &impl Fn(&ast::Path<'s>) -> bool,
        e: &mut errors::ManyError,
    ) -> Self {
        self.apply(op, exists, e);
        self
    }

    pub fn resolve_to_name<N, T>(
        &self,
        p: Path<'s>,
        map: &Trie<N, T, &'s str>,
    ) -> Result<N, IdentifierResolveError<'s>>
    where
        N: From<ast::Path<'s>>
            + Into<ast::Path<'s>>
            + std::hash::Hash
            + PartialEq
            + Eq
            + ast::TrieKey<&'s str>,
    {
        let rhits = self
            .resolve(p)
            .map_err(|_| IdentifierResolveError::PathBeyondRoot)?;
        let hits: Vec<_> = rhits
            .into_iter()
            .map(|p| N::from(p))
            .filter(|n| map.contains_key(n))
            .collect();
        if hits.len() == 1 {
            let mut hits = hits;
            Ok(hits.pop().unwrap())
        } else if hits.len() > 1 {
            let paths = hits.into_iter().map(|n| n.into()).collect();
            Err(IdentifierResolveError::Ambiguity(paths))
        } else {
            Err(IdentifierResolveError::NoHit)
        }
    }
}

#[derive(Clone, Debug)]
pub struct CaseArm<'s> {
    pub pattern: Pattern<'s>,
    pub guard: Option<Expr<'s>>,
    pub result: Expr<'s>,
}

pub use ast::Meta;

#[derive(Clone, Debug)]
pub enum Pattern<'s> {
    Construct(Path<'s>, Vec<Pattern<'s>>, Meta),
    Bind(&'s str, bool, Meta),
    Literal(Constant<'s>, Meta),
    Tuple(Vec<Pattern<'s>>, Meta),
}
pub use ast::{PrimitiveType, TypeVarId};

#[derive(Clone, Debug, Hash)]
pub struct Type<'s> {
    pub node: TypeNode<'s>,
    pub meta: Meta,
}

impl<'s> PartialEq for Type<'s> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}
impl<'s> Eq for Type<'s> {}

impl<'s> Type<'s> {
    pub fn new_var(id: TypeVarId, meta: Meta) -> Self {
        Self {
            node: TypeNode::Var(id),
            meta,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CallableType<'s> {
    pub params: Vec<Type<'s>>,
    pub ret: Box<Type<'s>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeNode<'s> {
    Callable(CallableType<'s>),
    ImpureCallable(CallableType<'s>),
    Generic(Path<'s>, Vec<Type<'s>>),
    Tuple(Vec<Type<'s>>),
    Primitive(PrimitiveType),
    Var(TypeVarId),
    Unit,
    Never,
    /// This is only used for parsing. During inference, any wildcard is replaced with a new variable
    WildCard,
}

#[derive(Debug, Clone)]
pub struct Expr<'s> {
    pub node: ExprNode<'s>,
    pub type_: Option<Type<'s>>,
    pub meta: ast::Meta,
}

impl<'s> Expr<'s> {
    pub fn new_untyped(node: ExprNode<'s>, meta: ast::Meta) -> Self {
        Self {
            node,
            meta,
            type_: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant<'s> {
    Int(i64),
    Float(&'s str),
    Str(String),
    Bool(bool),
    Char(&'s str),
}

#[derive(Clone, Debug)]
pub enum ExprNode<'s> {
    Apply {
        callee: Box<Expr<'s>>,
        args: Vec<Expr<'s>>,
    },
    IfElse {
        test: Box<Expr<'s>>,
        then: Box<Expr<'s>>,
        else_: Box<Expr<'s>>,
    },
    Case {
        operand: Box<Expr<'s>>,
        arms: Vec<CaseArm<'s>>,
    },
    LetPatIn {
        bind: Pattern<'s>,
        value: Box<Expr<'s>>,
        in_: Box<Expr<'s>>,
    },
    LetFnIn {
        identifier: &'s str,
        f: Function<'s>,
        in_: Box<Expr<'s>>,
    },
    Tuple(Vec<Expr<'s>>),
    Constant(Constant<'s>),
    Identifier(&'s str),
    Assign(Box<Expr<'s>>, Box<Expr<'s>>),
    Seq(Vec<Stmt<'s>>, Option<Box<Expr<'s>>>),
    Access(Box<Expr<'s>>, Box<Expr<'s>>),
    InNameSpace(Vec<NameSpaceOp<'s>>, Box<Expr<'s>>),
    Unit,
}

#[derive(Debug, Clone)]
pub enum StmtNode<'s> {
    LetDecl(Pattern<'s>, Expr<'s>),
    If { test: Expr<'s>, then: Expr<'s> },
    While { test: Expr<'s>, then: Expr<'s> },
    Expr(Expr<'s>),
}

#[derive(Debug, Clone)]
pub struct Stmt<'s> {
    pub(crate) node: StmtNode<'s>,
    pub(crate) meta: Meta,
}

impl<'s> Stmt<'s> {
    pub fn new(node: StmtNode<'s>, meta: Meta) -> Self {
        Self { node, meta }
    }
}

#[derive(Debug, Clone)]
pub struct Function<'s> {
    pub type_: Option<CallableType<'s>>,
    pub var_cnt: usize,
    pub params: Vec<&'s str>,
    pub param_metas: Vec<ast::Meta>,
    pub body: Box<Expr<'s>>,
    pub meta: ast::Meta,
    pub pure: bool,
    pub constrains: Vec<Constrain<'s>>,
}

#[derive(Clone, Debug, Hash)]
pub struct Constrain<'s> {
    pub rel_name: Path<'s>,
    pub args: Vec<Type<'s>>,
    pub meta: Meta,
}

#[derive(Clone, Debug)]
pub struct Instance<'s> {
    pub var_cnt: usize,
    pub rel: Constrain<'s>,
    pub constrains: Vec<Constrain<'s>>,
    pub meta: Meta,
}

impl<'s> Instance<'s> {
    pub fn rel_name(&self) -> Path<'s> {
        self.rel.rel_name.clone()
    }
}

#[derive(Clone, Debug)]
pub struct MethodSig<'s> {
    pub var_cnt: usize,
    pub type_: CallableType<'s>,
    pub pure: bool,
    pub constrains: Vec<Constrain<'s>>,
    pub primary_constrain: ast::Constrain<'s>,
    pub meta: Meta,
}

#[derive(Clone, Debug)]
pub struct Relation<'s> {
    pub name: ast::Path<'s>,
    /// Indicates whether some parameter is dependent on others
    pub dependent: Vec<usize>,
    pub constrains: Vec<Constrain<'s>>,
    pub arity: usize,
    pub f_sigs: std::collections::HashMap<&'s str, MethodSig<'s>>,
    pub meta: Meta,
}

pub trait RelationArity {
    fn arity(&self) -> usize;
}

impl<'s> RelationArity for Relation<'s> {
    fn arity(&self) -> usize {
        self.arity
    }
}

impl<'s> RelationArity for ast::Relation<'s> {
    fn arity(&self) -> usize {
        self.arity
    }
}

use std::collections::HashMap;

/// Vector of (module path, instance)
pub struct InstanceList<'s>(Vec<(ast::Path<'s>, Instance<'s>)>);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InstanceId(usize);

impl<'s> InstanceList<'s> {
    pub fn push(&mut self, path: ast::Path<'s>, ins: Instance<'s>) -> InstanceId {
        let id = self.0.len();
        self.0.push((path, ins));
        InstanceId(id)
    }
    pub fn module_for(&self, id: InstanceId) -> &ast::Path<'s> {
        &self.0[id.0].0
    }
    pub fn into_iter(self) -> impl Iterator<Item = (ast::Path<'s>, InstanceId, Instance<'s>)> {
        self.0
            .into_iter()
            .enumerate()
            .map(|(i, (p, ins))| (p, InstanceId(i), ins))
    }
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum FunctionId<'s> {
    Method(InstanceId, &'s str),
    Standard(ast::Path<'s>),
}

pub struct FunctionIdTable<'s, F> {
    methods: HashMap<(InstanceId, &'s str), F>,
    standard: Trie<ast::Path<'s>, F, &'s str>,
}

impl<'s, F> FunctionIdTable<'s, F> {
    pub fn new() -> Self {
        Self {
            methods: HashMap::new(),
            standard: Trie::new_branch(),
        }
    }

    pub fn insert(&mut self, k: FunctionId<'s>, f: F) -> Option<F> {
        match k {
            FunctionId::Method(ins_id, n) => self.methods.insert((ins_id, n), f),

            FunctionId::Standard(p) => self.standard.insert(p, f),
        }
    }

    pub fn get(&self, k: &FunctionId<'s>) -> Option<&F> {
        match k {
            FunctionId::Method(ins_id, n) => self.methods.get(&(*ins_id, *n)),
            FunctionId::Standard(p) => self.standard.get(p),
        }
    }

    pub fn get_mut(&mut self, k: &FunctionId<'s>) -> Option<&mut F> {
        match k {
            FunctionId::Method(ins_id, n) => self.methods.get_mut(&(*ins_id, *n)),
            FunctionId::Standard(p) => self.standard.get_mut(p),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (FunctionId<'s>, &F)> {
        let iter1 = self
            .methods
            .iter()
            .map(|((id, n), f)| (FunctionId::Method(*id, *n), f));
        let iter2 = self
            .standard
            .iter()
            .map(|(p, f)| (FunctionId::Standard(p), f));
        iter1.chain(iter2)
    }

    pub fn into_iter<'a>(self) -> impl Iterator<Item = (FunctionId<'s>, F)> + 'a
    where
        F: 'a,
        's: 'a,
    {
        let iter1 = self
            .methods
            .into_iter()
            .map(|((id, n), f)| (FunctionId::Method(id, n), f));
        let iter2 = self
            .standard
            .into_iter()
            .map(|(p, f)| (FunctionId::Standard(p), f));
        iter1.chain(iter2)
    }

    pub fn constains_standard_function<'r>(&self, x: ast::PathRefIter<'r, 's>) -> bool {
        self.standard.contains_path(x)
    }
}

pub struct BuiltinFunction<'s> {
    pub var_cnt: usize,
    pub type_: CallableType<'s>,
    pub meta: Meta,
    pub pure: bool,
}

pub use ast::Data;

pub struct Constructor<'s> {
    pub name: ast::ConstructorName<'s>,
    pub params: Vec<Type<'s>>,
    pub belongs_to: ast::DataName<'s>,
    pub meta: Meta,
}

use ast::Trie;

pub struct Symbols<'s> {
    pub functions: FunctionIdTable<'s, Function<'s>>,
    pub constructors: Trie<ast::ConstructorName<'s>, Constructor<'s>, &'s str>,
    pub datas: Trie<ast::DataName<'s>, Data<'s>, &'s str>,
    pub builtins: Trie<ast::BuiltinFunctionName<'s>, BuiltinFunction<'s>, &'s str>,
    pub relations: Trie<ast::RelationName<'s>, Relation<'s>, &'s str>,
    pub instances: InstanceList<'s>,
    pub methods: Trie<ast::FunctionName<'s>, ast::RelationName<'s>, &'s str>,
    pub module_namespaces: Trie<ast::Path<'s>, Vec<NameSpaceOp<'s>>, &'s str>,
}

impl<'s> Symbols<'s> {
    pub fn new() -> Self {
        Self {
            functions: FunctionIdTable::new(),
            constructors: Trie::new_branch(),
            datas: Trie::new_branch(),
            builtins: Trie::new_branch(),
            relations: Trie::new_branch(),
            instances: InstanceList::new(),
            methods: Trie::new_branch(),
            module_namespaces: Trie::new_branch(),
        }
    }
}
