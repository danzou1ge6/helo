#[derive(Debug, Clone, Copy)]
pub struct ExprId(usize);

impl From<usize> for ExprId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Clone, Hash, Debug)]
/// An absolute path
pub struct Path<'s>(pub Vec<&'s str>);

impl<'s> Path<'s> {
    pub fn parse_simple(s: &'s str) -> Self {
        let p = s.split('.').collect();
        Self(p)
    }

    pub fn push_many(&mut self, p: &Self) {
        self.0.extend_from_slice(&p.0)
    }

    pub fn from_raw(p: tast::Path<'s>) -> Self {
        Self(p.path)
    }
    pub fn split(mut self) -> (Self, &'s str) {
        let last = self.0.pop().unwrap();
        (self, last)
    }
    pub fn push(&mut self, id: &'s str) {
        self.0.push(id)
    }

    pub fn pushed(mut self, id: &'s str) -> Self {
        self.push(id);
        self
    }

    pub fn new<const N: usize>(p: [&'s str; N]) -> Self {
        Self(p.to_vec())
    }

    pub fn as_ref<'r>(&'r self) -> PathRef<'r, 's> {
        PathRef(&self.0)
    }

    pub fn into_iter<'r>(self) -> PathIter<'s> {
        PathIter { r: self, i: 0 }
    }

    pub fn iter<'r>(&'r self) -> PathRefIter<'r, 's> {
        PathRefIter {
            r: self.as_ref(),
            i: 0,
        }
    }

    pub fn head<'r>(&'r self) -> PathRef<'r, 's> {
        PathRef(&self.0[0..self.0.len() - 1])
    }
}

impl<'s, 'r> TrieKey<&'s str> for Path<'s> {
    type TP = PathIter<'s>;
    fn into_trie_key(self) -> PathIter<'s> {
        self.into_iter()
    }
    fn trie_key_ref<'a>(&'a self) -> PathRefIter<'a, 's> {
        self.iter()
    }
}

impl<'s> ConstructTriePath<&'s str> for Path<'s> {
    fn empty() -> Self {
        Self(Vec::new())
    }
    fn push(&mut self, k: &'s str) {
        self.0.push(k)
    }
}

macro_rules! path_instance {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name<'s>(pub Path<'s>);

        impl<'s> From<Path<'s>> for $name<'s> {
            fn from(value: Path<'s>) -> Self {
                Self(value)
            }
        }

        impl<'s> From<$name<'s>> for Path<'s> {
            fn from(value: $name<'s>) -> Self {
                value.0
            }
        }

        impl std::fmt::Display for $name<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl<'s> $name<'s> {
            pub fn id(&self) -> &'s str {
                self.0 .0.last().unwrap()
            }

            pub fn module_path<'r>(&'r self) -> PathRef<'r, 's> {
                let v = &self.0 .0;
                PathRef(&v[0..v.len() - 1])
            }

            pub fn in_module<const N: usize>(&self, p: [&'s str; N]) -> bool {
                self.0 .0.len() == N && self.0 .0.iter().zip(p.iter()).all(|(a, b)| a == b)
            }
        }

        impl<'s> TrieKey<&'s str> for $name<'s> {
            type TP = PathIter<'s>;
            fn trie_key_ref<'a>(&'a self) -> PathRefIter<'a, 's> {
                self.0.iter()
            }
            fn into_trie_key(self) -> PathIter<'s> {
                self.0.into_iter()
            }
        }

        impl<'s> ConstructTriePath<&'s str> for $name<'s> {
            fn empty() -> Self {
                Self(Path::empty())
            }
            fn push(&mut self, k: &'s str) {
                self.0.push(k)
            }
        }
    };
}

impl<'s> std::fmt::Display for Path<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        str_join_vec(f, ".", &self.0)
    }
}

pub fn paths_to_string<'s>(p: &[Path<'s>]) -> String {
    let mut buf = String::new();
    let mut f = std::fmt::Formatter::new(&mut buf);
    str_join_vec(&mut f, ", ", p).unwrap();
    buf
}

impl<'s> PartialEq for Path<'s> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<'s> Eq for Path<'s> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub usize);

impl std::fmt::Display for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

impl std::default::Default for LocalId {
    fn default() -> Self {
        Self(0)
    }
}

impl std::ops::Add<usize> for LocalId {
    type Output = Self;
    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl From<usize> for LocalId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<LocalId> for usize {
    fn from(value: LocalId) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CapturedId(pub usize);

impl From<usize> for CapturedId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ClosureId {
    span: (usize, usize),
    file: Arc<String>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum FunctionId<'s> {
    Method(InstanceId<'s>, &'s str),
    Standard(Path<'s>),
    Closure(ClosureId),
}

impl<'s> FunctionId<'s> {
    pub fn of_standard(n: FunctionName<'s>) -> Self {
        Self::Standard(Path::from(n))
    }
    pub fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ins_tab: &InstanceIdTable<'s, Instance<'s>>,
    ) -> std::fmt::Result {
        match self {
            Self::Method(ins, name) => {
                write!(f, "{}<", ins.rel_name)?;
                let ins = ins_tab.get(ins).unwrap();
                str_join_vec(f, ", ", &ins.rel.args)?;
                write!(f, ">::{}", name)
            }
            Self::Standard(name) => write!(f, "{}", name),
            Self::Closure(cid) => write!(f, "closure@{}:{}-{}", cid.file, cid.span.0, cid.span.1),
        }
    }

    pub fn to_string(&self, ins_tab: &InstanceIdTable<'s, Instance<'s>>) -> String {
        let mut buf = String::new();
        let mut formatter = core::fmt::Formatter::new(&mut buf);
        self.fmt(&mut formatter, ins_tab).unwrap();
        buf
    }
}

impl<'s> FunctionId<'s> {
    pub fn of_method(ins: InstanceId<'s>, name: &'s str) -> Self {
        Self::Method(ins, name)
    }
    pub fn of_closure_at(meta: &Meta) -> Self {
        Self::Closure(ClosureId {
            span: meta.span,
            file: meta.file_name.clone(),
        })
    }
}

use crate::parse::tast;

#[derive(Debug, Clone)]
pub enum ExprNode<'s> {
    Apply {
        callee: ExprId,
        args: Vec<ExprId>,
    },
    IfElse {
        test: ExprId,
        then: ExprId,
        else_: ExprId,
    },
    Case {
        operand: ExprId,
        arms: Vec<CaseArm<'s>>,
    },
    LetIn {
        bind: LocalId,
        value: ExprId,
        in_: ExprId,
    },
    LetPatIn {
        bind: Pattern<'s>,
        value: ExprId,
        in_: ExprId,
    },
    MakeClosure(FunctionId<'s>),
    Constructor(ConstructorName<'s>),
    Builtin(BuiltinFunctionName<'s>),
    UserFunction(FunctionName<'s>),
    Tuple(Vec<ExprId>),
    Captured {
        id: CapturedId,
        is_self: bool,
        mutable: bool,
    },
    Constant(Constant<'s>),
    /// .1 indicates mutablility
    Local(LocalId, bool),
    Seq(VecDeque<Stmt>, Option<ExprId>),
    Assign(ExprId, ExprId),
    Never,
    Unit,
}

#[derive(Debug, Clone)]
pub enum StmtNode {
    If { test: ExprId, then: ExprId },
    While { test: ExprId, then: ExprId },
    Expr(ExprId),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub(crate) node: StmtNode,
    pub(crate) meta: Meta,
}

impl Stmt {
    pub fn new(node: StmtNode, meta: Meta) -> Self {
        Self { node, meta }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant<'s> {
    Int(i64),
    Float(&'s str),
    Str(String),
    Bool(bool),
    Char(char),
}

impl<'s> Constant<'s> {
    pub fn type_(&self) -> PrimitiveType {
        use Constant::*;
        match self {
            Int(_) => PrimitiveType::Int,
            Float(_) => PrimitiveType::Float,
            Bool(_) => PrimitiveType::Bool,
            Str(_) => PrimitiveType::Str,
            Char(_) => PrimitiveType::Char,
        }
    }
}

#[derive(Debug)]
pub struct Expr<'s> {
    pub node: ExprNode<'s>,
    pub type_: Option<(Type<'s>, Meta)>,
    pub meta: Meta,
}

impl<'s> Expr<'s> {
    pub fn new_untyped(node: ExprNode<'s>, meta: Meta) -> Self {
        Self {
            node,
            meta,
            type_: None,
        }
    }
    pub fn new(node: ExprNode<'s>, meta: Meta, type_: Option<(Type<'s>, Meta)>) -> Self {
        Self { node, meta, type_ }
    }
    pub fn new_never(meta: Meta) -> Self {
        Self {
            node: ExprNode::Never,
            type_: None,
            meta,
        }
    }
}

#[derive(Debug)]
pub struct Function<'s> {
    pub type_: Option<CallableType<'s>>,
    pub var_cnt: usize,
    pub local_cnt: usize,
    pub arity: usize,
    pub body: ExprId,
    pub meta: Meta,
    pub param_metas: Vec<Meta>,
    pub captures: Vec<LocalId>,
    pub captures_meta: Vec<Meta>,
    pub pure: bool,
    pub constrains: Vec<Constrain<'s>>,
}

#[derive(Debug, Clone)]
pub struct CaseArm_<'s, Id> {
    pub pattern: Pattern<'s>,
    pub guard: Option<Id>,
    pub result: Id,
}

pub type CaseArm<'s> = CaseArm_<'s, ExprId>;

#[derive(Clone)]
pub enum Pattern<'s> {
    Construct(ConstructorName<'s>, Vec<Pattern<'s>>, Meta),
    Bind(LocalId, Meta),
    Literal(Constant<'s>, Meta),
    Tuple(Vec<Pattern<'s>>, Meta),
}

impl<'s> std::fmt::Display for Pattern<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Pattern::*;
        match self {
            Construct(name, args, _) => {
                write!(f, "{name}(")?;
                str_join_vec(f, ", ", args)?;
                write!(f, ")")
            }
            Bind(local, _) => write!(f, "{local}"),
            Literal(lit, _) => write!(f, "{lit:?}"),
            Tuple(elems, _) => {
                write!(f, "(")?;
                str_join_vec(f, ", ", elems)?;
                write!(f, ")")
            }
        }
    }
}

impl<'s> std::fmt::Debug for Pattern<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub struct Constructor<'s> {
    pub name: ConstructorName<'s>,
    pub params: Vec<Type<'s>>,
    pub belongs_to: DataName<'s>,
    pub meta: Meta,
}

impl<'s> Pattern<'s> {
    pub fn meta(&self) -> &Meta {
        match self {
            Pattern::Bind(_, meta) => meta,
            Pattern::Construct(_, _, meta) => meta,
            Pattern::Literal(_, meta) => meta,
            Pattern::Tuple(_, meta) => meta,
        }
    }
    pub fn unwrap_literal(self) -> (Constant<'s>, Meta) {
        match self {
            Pattern::Literal(c, m) => (c, m),
            _ => panic!("Called `unwrap_literal` on a pattern {:?}", self),
        }
    }
    pub fn unwrap_construct(self) -> (ConstructorName<'s>, Vec<Pattern<'s>>, Meta) {
        match self {
            Pattern::Construct(c, args, m) => (c, args, m),
            _ => panic!("Called `unwrap_construct` on a pattern {:?}", self),
        }
    }
    pub fn unwrap_tuple(self) -> (Vec<Pattern<'s>>, Meta) {
        match self {
            Pattern::Tuple(args, m) => (args, m),
            _ => panic!("Called `unwrap_tuple` on a pattern {:?}", self),
        }
    }

    pub fn validate(&self, symbols: &Symbols<'s>) -> Result<(), miette::Report> {
        use crate::errors;
        match self {
            Pattern::Bind(_, _) => Ok(()),
            Pattern::Literal(_, _) => Ok(()),
            Pattern::Construct(constructor, args, m) => {
                if let Some(_) = symbols.constructors.get(&constructor) {
                    for a in args {
                        a.validate(symbols)?;
                    }
                    Ok(())
                } else {
                    Err(miette::Report::new(errors::ConstructorNotFound::new(
                        constructor.clone(),
                        m,
                    )))
                }
            }
            Pattern::Tuple(v, _) => {
                for elem in v {
                    elem.validate(symbols)?;
                }
                Ok(())
            }
        }
    }
    /// Check if this pattern is refutable. WARNING: Pattern must be validated before hand
    pub fn inrefutable(&self, symbols: &Symbols<'s>) -> bool {
        match self {
            Pattern::Construct(constructor, args, _) => {
                let c = symbols.constructors.get(&constructor).unwrap();
                let data = symbols.datas.get(&c.belongs_to).unwrap();
                data.constructors.len() == 1 && args.iter().all(|pat| pat.inrefutable(symbols))
            }
            Self::Tuple(elements, _) => elements.iter().all(|pat| pat.inrefutable(symbols)),
            Self::Literal(_, _) => true,
            Self::Bind(_, _) => true,
        }
    }
}

path_instance!(DataName);
path_instance!(ConstructorName);
path_instance!(FunctionName);
path_instance!(RelationName);
path_instance!(BuiltinFunctionName);

pub struct Data<'s> {
    pub name: &'s str,
    pub kind_arity: usize,
    pub constructors: Vec<ConstructorName<'s>>,
    pub meta: Meta,
    pub generic_metas: Vec<Meta>,
}

use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::marker::PhantomData;
use std::sync::Arc;

#[derive(Clone, Debug, Hash)]
pub struct Meta {
    pub span: (usize, usize),
    pub file_name: Arc<String>,
    pub src: Arc<String>,
}

impl Meta {
    pub fn named_source(&self) -> miette::NamedSource {
        let file: &String = self.file_name.borrow();
        miette::NamedSource::new(file, self.src.clone())
    }

    pub fn span(&self) -> miette::SourceSpan {
        self.span.into()
    }

    pub fn closure_id(&self) -> String {
        format!(
            "[closure@{}:{}-{}]",
            self.file_name, self.span.0, self.span.1
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub usize);

impl TypeVarId {
    pub fn offset(self, by: usize) -> Self {
        Self(self.0 + by)
    }
}

impl std::ops::Add for TypeVarId {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl From<usize> for TypeVarId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

/// Signatures for a method.
///
/// For relation methods,
/// `var_cnt` is number of generic parameters in relation definition plus
/// those in function signature. `type_` should contain both kind of generic parameters.
#[derive(Clone, Debug)]
pub struct MethodSig<'s> {
    pub var_cnt: usize,
    pub type_: CallableType<'s>,
    pub pure: bool,
    pub constrains: Vec<Constrain<'s>>,
    pub primary_constrain: Constrain<'s>,
    pub meta: Meta,
}

impl<'s> std::fmt::Display for MethodSig<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.type_.fmt(f)?;
        write!(f, " where ")?;
        str_join_vec(f, " + ", &self.constrains)
    }
}

impl<'s> TypeApply<'s> for MethodSig<'s> {
    fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> Self {
        let type_ = self.type_.apply(selector, f);
        let constrains = self
            .constrains
            .iter()
            .map(|x| x.apply(selector, f))
            .collect();
        let primary_constrain = self.primary_constrain.apply(selector, f);
        MethodSig {
            type_,
            constrains,
            primary_constrain,
            pure: self.pure,
            meta: self.meta.clone(),
            var_cnt: self.var_cnt,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Relation<'s> {
    pub name: RelationName<'s>,
    /// Indicates whether some parameter is dependent on others
    pub dependent: Vec<usize>,
    pub constrains: Vec<Constrain<'s>>,
    pub arity: usize,
    pub f_sigs: HashMap<&'s str, MethodSig<'s>>,
    pub meta: Meta,
}

#[derive(Clone, Debug, Hash)]
pub struct Constrain<'s> {
    pub rel_name: RelationName<'s>,
    pub args: Vec<Type<'s>>,
    pub meta: Meta,
}

impl<'s> std::fmt::Display for Constrain<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.rel_name)?;
        str_join_vec(f, ", ", &self.args[..])
    }
}

impl<'s> PartialEq for Constrain<'s> {
    fn eq(&self, other: &Self) -> bool {
        self.rel_name == other.rel_name && self.args == other.args
    }
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}
impl<'s> Eq for Constrain<'s> {}

impl<'s> TypeApply<'s> for Constrain<'s> {
    fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> Self {
        let args = self.args.iter().map(|t| t.apply(selector, f)).collect();
        Constrain {
            rel_name: self.rel_name.clone(),
            args,
            meta: self.meta.clone(),
        }
    }
}

impl<'s> TypeApplyResult<'s> for Constrain<'s> {
    fn apply_result<E>(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Result<Type<'s>, E>,
    ) -> Result<Self, E> {
        let args = Type::apply_many_result(self.args.iter(), selector, f)?;
        Ok(Constrain {
            rel_name: self.rel_name.clone(),
            args,
            meta: self.meta.clone(),
        })
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstanceId<'s> {
    pub rel_name: RelationName<'s>,
    pub n: usize,
}

#[derive(Clone, Debug)]
pub struct Instance<'s> {
    pub var_cnt: usize,
    pub rel: Constrain<'s>,
    pub constrains: Vec<Constrain<'s>>,
    pub meta: Meta,
}

impl<'s> Instance<'s> {
    pub fn rel_name(&self) -> &RelationName<'s> {
        &self.rel.rel_name
    }
}

impl<'s> TypeApply<'s> for Instance<'s> {
    fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> Self {
        let rel = self.rel.apply(selector, f);
        let constrains = self
            .constrains
            .iter()
            .map(|c| c.apply(selector, f))
            .collect();
        Self {
            rel,
            constrains,
            ..self.clone()
        }
    }
}

impl<'s> Relation<'s> {
    pub fn primary_constrain(&self) -> Constrain<'s> {
        Constrain {
            rel_name: self.name.clone(),
            args: (0..self.arity)
                .map(|x| Type::new_var(TypeVarId(x)))
                .collect(),
            meta: self.meta.clone(),
        }
    }
    pub fn method_sig(&self, x: &str) -> &MethodSig<'s> {
        &self.f_sigs[x]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeNode<'s> {
    Callable(CallableType<'s>),
    ImpureCallable(CallableType<'s>),
    Generic(DataName<'s>, Vec<Type<'s>>),
    Tuple(Vec<Type<'s>>),
    Primitive(PrimitiveType),
    Var(TypeVarId),
    Unit,
    Never,
    /// This is only used for parsing. During inference, any wildcard is replaced with a new variable
    WildCard,
}

impl<'s> std::fmt::Display for TypeNode<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TypeNode::*;
        match self {
            Callable(c) => {
                write!(f, "{c}")
            }
            ImpureCallable(c) => {
                write!(f, "^{c}")
            }
            Generic(DataName(template), args) => {
                if args.len() == 0 {
                    write!(f, "{}", template)
                } else {
                    write!(f, "{}[", template)?;
                    str_join_vec(f, ", ", args)?;
                    write!(f, "]")
                }
            }
            Tuple(vec) => {
                write!(f, "(")?;
                str_join_vec(f, ", ", vec)?;
                write!(f, ")")
            }
            Primitive(p) => write!(f, "{}", p),
            Var(v) => write!(f, "'{}", v.0),
            Unit => write!(f, "()"),
            Never => write!(f, "!"),
            WildCard => write!(f, "*"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CallableType<'s> {
    pub params: Vec<Type<'s>>,
    pub ret: Box<Type<'s>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionType<'s> {
    pub params: Vec<Type<'s>>,
    pub ret: Box<Type<'s>>,
    pub captures: Vec<Type<'s>>,
}

impl<'s> FunctionType<'s> {
    pub fn collect_vars(&self, vars: &mut HashSet<TypeVarId>) {
        self.params.iter().for_each(|p| p.collect_vars(vars));
        self.captures.iter().for_each(|c| c.collect_vars(vars));
        self.ret.collect_vars(vars);
    }
    pub fn substitute_vars(&mut self, values: &impl Fn(TypeVarId) -> Type<'s>) {
        self.params.iter_mut().for_each(|p| {
            *p = p.substitute_vars(values);
        });
        self.captures.iter_mut().for_each(|p| {
            *p = p.substitute_vars(values);
        });
        self.ret = Box::new(self.ret.substitute_vars(values));
    }
}

impl<'s> From<FunctionType<'s>> for CallableType<'s> {
    fn from(value: FunctionType<'s>) -> Self {
        Self {
            params: value.params,
            ret: value.ret,
        }
    }
}
impl<'s> From<CallableType<'s>> for FunctionType<'s> {
    fn from(value: CallableType<'s>) -> Self {
        Self {
            params: value.params,
            ret: value.ret,
            captures: vec![],
        }
    }
}

pub fn str_join_vec<T>(f: &mut std::fmt::Formatter<'_>, j: &str, slice: &[T]) -> std::fmt::Result
where
    T: std::fmt::Display,
{
    if let Some(last) = slice.last() {
        for p in slice.iter().take(slice.len() - 1) {
            write!(f, "{p}{j}")?;
        }
        write!(f, "{last}")?;
    }
    std::fmt::Result::Ok(())
}

impl<'s> std::fmt::Display for CallableType<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        str_join_vec(f, ", ", &self.params)?;
        write!(f, "] -> {}", &self.ret)
    }
}

impl<'s> std::fmt::Display for FunctionType<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        str_join_vec(f, ", ", &self.params)?;
        if !self.captures.is_empty() {
            write!(f, "][")?;
            str_join_vec(f, ", ", &self.captures)?;
        }
        write!(f, "] -> {}", &self.ret)
    }
}

impl<'s> TypeApply<'s> for CallableType<'s> {
    fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> Self {
        let params = self.params.iter().map(|x| x.apply(selector, f)).collect();
        let ret = self.ret.apply(selector, f);
        CallableType {
            params,
            ret: Box::new(ret),
        }
    }
}

impl<'s> TypeApply<'s> for FunctionType<'s> {
    fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> Self {
        let params = self.params.iter().map(|x| x.apply(selector, f)).collect();
        let captures = self.captures.iter().map(|x| x.apply(selector, f)).collect();
        let ret = self.ret.apply(selector, f);
        FunctionType {
            params,
            captures,
            ret: Box::new(ret),
        }
    }
}

impl<'s> TypeApplyResult<'s> for FunctionType<'s> {
    fn apply_result<E>(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Result<Type<'s>, E>,
    ) -> Result<Self, E>
    where
        Self: Sized,
    {
        let params = Type::apply_many_result(self.params.iter(), selector, f)?;
        let ret = Box::new(self.ret.apply_result(selector, f)?);
        let captures = Type::apply_many_result(self.captures.iter(), selector, f)?;
        Ok(FunctionType {
            params,
            ret,
            captures,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Int,
    Float,
    Str,
    Bool,
    Char,
}

impl PrimitiveType {
    pub fn name(&self) -> &'static str {
        use PrimitiveType::*;
        match self {
            Int => "Int",
            Float => "Float",
            Str => "Str",
            Bool => "Bool",
            Char => "Char",
        }
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Type<'s> {
    pub node: TypeNode<'s>,
}

pub trait TypeApply<'s> {
    fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> Self;

    fn substitute_vars_with_nodes(&self, mut nodes: impl FnMut(TypeVarId) -> TypeNode<'s>) -> Self
    where
        Self: Sized,
    {
        self.apply(
            &|t| matches!(t.node, TypeNode::Var(_)),
            &mut |t| match t.node {
                TypeNode::Var(v) => Type { node: nodes(v) },
                _ => unreachable!(),
            },
        )
    }

    fn substitute_vars(&self, mut nodes: impl FnMut(TypeVarId) -> Type<'s>) -> Self
    where
        Self: Sized,
    {
        self.apply(
            &|t| matches!(t.node, TypeNode::Var(_)),
            &mut |t| match t.node {
                TypeNode::Var(v) => nodes(v),
                _ => unreachable!(),
            },
        )
    }
}

impl<'s, T> TypeApply<'s> for Vec<T>
where
    T: TypeApply<'s>,
{
    fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> Self {
        self.iter().map(|x| x.apply(selector, f)).collect()
    }
}

pub trait TypeApplyResult<'s> {
    fn apply_result<E>(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Result<Type<'s>, E>,
    ) -> Result<Self, E>
    where
        Self: Sized;
}

impl<'s> TypeApply<'s> for Type<'s> {
    fn apply(&self, selector: &impl Fn(&Self) -> bool, f: &mut impl FnMut(&Self) -> Self) -> Self {
        use TypeNode::*;
        fn apply_many<'a, 's>(
            many: impl Iterator<Item = &'a Type<'s>>,
            selector: &impl Fn(&Type<'s>) -> bool,
            f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
        ) -> Vec<Type<'s>>
        where
            's: 'a,
        {
            many.map(|x| x.apply(selector, f)).collect()
        }
        if selector(self) {
            return f(self);
        }
        let node = match &self.node {
            Primitive(p) => Primitive(*p),
            Unit => Unit,
            Generic(template, args) => {
                Generic(template.clone(), apply_many(args.iter(), selector, f))
            }
            Callable(v) => Callable(CallableType {
                params: apply_many(v.params.iter(), selector, f),
                ret: Box::new(v.ret.apply(selector, f)),
            }),
            ImpureCallable(v) => ImpureCallable(CallableType {
                params: apply_many(v.params.iter(), selector, f),
                ret: Box::new(v.ret.apply(selector, f)),
            }),
            Tuple(v) => Tuple(apply_many(v.iter(), selector, f)),
            Var(v) => Var(*v),
            Never => Never,
            WildCard => WildCard,
        };
        Type { node }
    }
}

impl<'s> TypeNode<'s> {
    pub fn impure(&self) -> bool {
        match self {
            TypeNode::Callable(..) => false,
            TypeNode::ImpureCallable(..) => true,
            _ => panic!(
                "Only callable types have the notion of purity, but not {}",
                self
            ),
        }
    }
    pub fn unwrap_callable(self) -> CallableType<'s> {
        match self {
            Self::Callable(c) => c,
            Self::ImpureCallable(c) => c,
            other => panic!("Called `unwrap_callable` on type {}", other),
        }
    }
    pub fn is_unit(&self) -> bool {
        matches!(self, TypeNode::Unit)
    }
}

impl<'s> TypeApplyResult<'s> for Type<'s> {
    fn apply_result<E>(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Result<Type<'s>, E>,
    ) -> Result<Type<'s>, E> {
        use TypeNode::*;
        if selector(self) {
            return f(self);
        }
        let node = match &self.node {
            Primitive(p) => Primitive(*p),
            Unit => Unit,
            Generic(template, args) => Generic(
                template.clone(),
                Type::apply_many_result(args.iter(), selector, f)?,
            ),
            Callable(v) => Callable(CallableType {
                params: Type::apply_many_result(v.params.iter(), selector, f)?,
                ret: Box::new(v.ret.apply_result(selector, f)?),
            }),
            ImpureCallable(v) => ImpureCallable(CallableType {
                params: Type::apply_many_result(v.params.iter(), selector, f)?,
                ret: Box::new(v.ret.apply_result(selector, f)?),
            }),
            Tuple(v) => Tuple(Type::apply_many_result(v.iter(), selector, f)?),
            Var(v) => Var(*v),
            Never => Never,
            WildCard => WildCard,
        };
        Ok(Type { node })
    }
}

impl<'s> Type<'s> {
    pub fn new(node: TypeNode<'s>) -> Self {
        Self { node }
    }
    pub fn new_wildcard() -> Self {
        Self {
            node: TypeNode::WildCard,
        }
    }
    pub fn new_var(id: TypeVarId) -> Self {
        Self {
            node: TypeNode::Var(id),
        }
    }

    pub fn new_never() -> Self {
        Self {
            node: TypeNode::Never,
        }
    }

    pub fn new_unit() -> Self {
        Self {
            node: TypeNode::Unit,
        }
    }

    fn apply_many_result<'a, E>(
        many: impl Iterator<Item = &'a Type<'s>>,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Result<Type<'s>, E>,
    ) -> Result<Vec<Type<'s>>, E>
    where
        's: 'a,
    {
        let mut r = vec![];
        for x in many {
            r.push(x.apply_result(selector, f)?);
        }
        Ok(r)
    }

    pub fn walk(&self, f: &mut impl FnMut(&Type<'s>) -> ()) {
        f(self);
        use TypeNode::*;
        match &self.node {
            Generic(_, args) => {
                for a in args {
                    a.walk(f);
                }
            }
            Tuple(v) => {
                for elem in v {
                    elem.walk(f);
                }
            }
            Callable(c) | ImpureCallable(c) => {
                for p in &c.params {
                    p.walk(f);
                }
                c.ret.walk(f);
            }
            _ => {}
        }
    }

    pub fn collect_vars(&self, vars: &mut HashSet<TypeVarId>) {
        self.walk(&mut |t| match &t.node {
            TypeNode::Var(v) => {
                vars.insert(*v);
            }
            _ => {}
        });
    }

    /// Replace variable_i in the tree with `values(i)`
    pub fn substitute_vars(&self, values: &impl Fn(TypeVarId) -> Type<'s>) -> Type<'s> {
        self.apply(
            &|t| matches!(t.node, TypeNode::Var(_)),
            &mut |t| match t.node {
                TypeNode::Var(v) => values(v),
                _ => unreachable!(),
            },
        )
    }

    pub fn offset_vars(&self, offset: TypeVarId) -> Type<'s> {
        self.apply(
            &|t| matches!(t.node, TypeNode::Var(_)),
            &mut |t| match t.node {
                TypeNode::Var(v) => Type {
                    node: TypeNode::Var(v + offset),
                },
                _ => unreachable!(),
            },
        )
    }
}

impl<'s> std::fmt::Display for Type<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

impl<'s> std::fmt::Debug for Type<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.node)
    }
}

pub struct BuiltinFunction<'s> {
    pub var_cnt: usize,
    pub type_: CallableType<'s>,
    pub meta: Meta,
    pub pure: bool,
}

pub struct InstanceIdTable<'s, I>(Trie<RelationName<'s>, Vec<I>, &'s str>);

impl<'s, I> InstanceIdTable<'s, I> {
    pub fn get<'a, 'b>(&'a self, id: &'b InstanceId<'s>) -> Option<&'a I> {
        let v = self.0.get(&id.rel_name)?;
        v.get(id.n)
    }
    pub fn get_mut<'a, 'b>(&'a mut self, id: &'b InstanceId<'s>) -> Option<&'a mut I> {
        let v = self.0.get_mut(&id.rel_name)?;
        v.get_mut(id.n)
    }
    pub fn new() -> Self {
        Self(Trie::new_branch())
    }
    pub fn of_rel(&self, rel_name: &RelationName<'s>) -> &[I] {
        &self.0.get(rel_name).unwrap()
    }
    pub fn insert(&mut self, rel_name: RelationName<'s>, instance: I) -> InstanceId<'s> {
        if self.0.contains_key(&rel_name) {
            let n = self.0.get(&rel_name).unwrap().len();
            self.0.get_mut(&rel_name).unwrap().push(instance);
            InstanceId { rel_name, n }
        } else {
            self.0.insert(rel_name.clone(), vec![instance]);
            InstanceId { rel_name, n: 0 }
        }
    }
}
impl<'s> InstanceIdTable<'s, Instance<'s>> {
    pub fn module_for<'a>(&'a self, id: &InstanceId<'s>) -> PathRef<'a, 's> {
        self.get(id).unwrap().rel_name().module_path()
    }
    pub fn iter(&self) -> impl Iterator<Item = (InstanceId<'s>, &Instance<'s>)> {
        self.0
            .iter()
            .map(|(name, v)| {
                v.iter().enumerate().map(move |(n, ins)| {
                    (
                        InstanceId {
                            rel_name: name.clone(),
                            n,
                        },
                        ins,
                    )
                })
            })
            .flatten()
    }
}

pub struct FunctionIdTable<'s, F> {
    methods: HashMap<(usize, &'s str), Trie<RelationName<'s>, F, &'s str>>,
    standard: Trie<Path<'s>, F, &'s str>,
    closure: HashMap<ClosureId, F>,
}

impl<'s, F> FunctionIdTable<'s, F> {
    pub fn new() -> Self {
        Self {
            methods: HashMap::new(),
            standard: Trie::new_branch(),
            closure: HashMap::new(),
        }
    }

    pub fn insert(&mut self, k: FunctionId<'s>, f: F) -> Option<F> {
        match k {
            FunctionId::Closure(cid) => self.closure.insert(cid, f),
            FunctionId::Method(ins_id, n) => {
                if let Some(ins_tab) = self.methods.get_mut(&(ins_id.n, n)) {
                    ins_tab.insert(ins_id.rel_name, f)
                } else {
                    let mut t = Trie::new_branch();
                    let r = t.insert(ins_id.rel_name, f);
                    self.methods.insert((ins_id.n, n), t);
                    r
                }
            }
            FunctionId::Standard(p) => self.standard.insert(p, f),
        }
    }

    pub fn get(&self, k: &FunctionId<'s>) -> Option<&F> {
        match k {
            FunctionId::Closure(cid) => self.closure.get(cid),
            FunctionId::Method(ins_id, n) => self
                .methods
                .get(&(ins_id.n, n))
                .map(|t| t.get(&ins_id.rel_name))
                .flatten(),
            FunctionId::Standard(p) => self.standard.get(p),
        }
    }

    pub fn get_mut(&mut self, k: &FunctionId<'s>) -> Option<&mut F> {
        match k {
            FunctionId::Closure(cid) => self.closure.get_mut(cid),
            FunctionId::Method(ins_id, n) => self
                .methods
                .get_mut(&(ins_id.n, n))
                .map(|t| t.get_mut(&ins_id.rel_name))
                .flatten(),
            FunctionId::Standard(p) => self.standard.get_mut(p),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (FunctionId<'s>, &F)> {
        let iter1 = self
            .methods
            .iter()
            .map(|((n, name), t)| {
                t.iter().map(|(rel_name, f)| {
                    (FunctionId::Method(InstanceId { rel_name, n: *n }, *name), f)
                })
            })
            .flatten();
        let iter2 = self
            .standard
            .iter()
            .map(|(p, f)| (FunctionId::Standard(p), f));
        let iter3 = self
            .closure
            .iter()
            .map(|(cid, f)| (FunctionId::Closure(cid.clone()), f));
        iter1.chain(iter2).chain(iter3)
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (FunctionId<'s>, &mut F)> {
        let iter1 = self
            .methods
            .iter_mut()
            .map(|((n, name), t)| {
                t.iter_mut().map(|(rel_name, f)| {
                    (FunctionId::Method(InstanceId { rel_name, n: *n }, *name), f)
                })
            })
            .flatten();
        let iter2 = self
            .standard
            .iter_mut()
            .map(|(p, f)| (FunctionId::Standard(p), f));
        let iter3 = self
            .closure
            .iter_mut()
            .map(|(cid, f)| (FunctionId::Closure(cid.clone()), f));
        iter1.chain(iter2).chain(iter3)
    }

    pub fn into_iter<'a>(self) -> impl Iterator<Item = (FunctionId<'s>, F)> + 'a
    where
        F: 'a,
        's: 'a,
    {
        let iter1 = self
            .methods
            .into_iter()
            .map(|((n, name), t)| {
                t.into_iter().map(move |(rel_name, f)| {
                    (FunctionId::Method(InstanceId { rel_name, n }, name), f)
                })
            })
            .flatten();
        let iter2 = self
            .standard
            .into_iter()
            .map(|(p, f)| (FunctionId::Standard(p), f));
        let iter3 = self
            .closure
            .into_iter()
            .map(|(cid, f)| (FunctionId::Closure(cid), f));
        iter1.chain(iter2).chain(iter3)
    }

    pub fn contains_key(&self, k: &FunctionId<'s>) -> bool {
        self.get(k).is_some()
    }

    pub fn keys<'a>(&'a self) -> impl Iterator<Item = FunctionId<'s>> + 'a {
        self.iter().map(|(fid, _)| fid)
    }
}

impl<'s, F> std::ops::Index<&FunctionId<'s>> for FunctionIdTable<'s, F> {
    type Output = F;
    fn index(&self, index: &FunctionId<'s>) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<'s, F> std::ops::IndexMut<&FunctionId<'s>> for FunctionIdTable<'s, F> {
    fn index_mut(&mut self, index: &FunctionId<'s>) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

pub type InstanceTable<'s> = InstanceIdTable<'s, Instance<'s>>;
pub type FunctionTable<'s> = FunctionIdTable<'s, Function<'s>>;

pub struct Symbols<'s> {
    pub functions: FunctionTable<'s>,
    pub constructors: Trie<ConstructorName<'s>, Constructor<'s>, &'s str>,
    pub datas: Trie<DataName<'s>, Data<'s>, &'s str>,
    pub builtins: Trie<BuiltinFunctionName<'s>, BuiltinFunction<'s>, &'s str>,
    pub relations: Trie<RelationName<'s>, Relation<'s>, &'s str>,
    pub instances: InstanceTable<'s>,
    pub methods: Trie<FunctionName<'s>, RelationName<'s>, &'s str>,
}

impl<'s> Symbols<'s> {
    pub fn validate_type(&self, type_: &Type<'s>, meta: &Meta) -> Result<(), miette::Report> {
        use crate::errors;
        use TypeNode::*;
        match &type_.node {
            Generic(template, args) => {
                if let None = self.datas.get(&template) {
                    return Err(miette::Report::new(errors::DataNotFound::new(
                        template.clone(),
                        meta,
                    )));
                }
                if args.len() != self.datas.get(&template).unwrap().kind_arity {
                    return Err(miette::Report::new(errors::WrongNumberOfArgs::new(
                        self.datas.get(&template).unwrap().kind_arity,
                        meta,
                    )));
                }
                for t in args {
                    self.validate_type(t, meta)?;
                }
                Ok(())
            }
            Callable(callable) => {
                for p in &callable.params {
                    self.validate_type(p, meta)?;
                }
                self.validate_type(&callable.ret, meta)?;
                Ok(())
            }
            ImpureCallable(callable) => {
                for p in &callable.params {
                    self.validate_type(p, meta)?;
                }
                self.validate_type(&callable.ret, meta)?;
                Ok(())
            }
            Tuple(v) => {
                for elem in v {
                    self.validate_type(elem, meta)?;
                }
                Ok(())
            }
            Primitive(_) | Var(_) | Unit | Never | WildCard => Ok(()),
        }
    }
    pub fn validate_constrain(&self, c: &Constrain<'s>) -> Result<(), miette::Report> {
        use crate::errors;
        if !self.relations.contains_key(&c.rel_name) {
            return Err(miette::Report::new(errors::RelationNonExists::new(&c.meta)));
        }

        let rel_arity = self.relations.get(&c.rel_name).unwrap().arity;
        if rel_arity != c.args.len() {
            return Err(miette::Report::new(errors::RelationArityWrong::new(
                &c.meta, rel_arity,
            )));
        }

        for t in c.args.iter() {
            self.validate_type(t, &c.meta)?;
        }

        Ok(())
    }
    pub fn validate_callable_type(
        &self,
        type_: &CallableType<'s>,
        meta: &Meta,
    ) -> Result<(), miette::Report> {
        for p in &type_.params {
            self.validate_type(p, meta)?;
        }
        self.validate_type(&type_.ret, meta)
    }
}

pub enum Trie<K, V, NK> {
    Branch(Option<V>, HashMap<NK, Trie<K, V, NK>>),
    Leaf(V),
    _Phantom(PhantomData<K>),
}

impl<K, V, NK> Trie<K, V, NK> {
    pub fn new_branch() -> Self {
        Self::Branch(None, HashMap::new())
    }
}

impl<K, V, NK> FromIterator<(K, V)> for Trie<K, V, NK>
where
    K: TrieKey<NK>,
    NK: Hash + Eq,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut t = Self::new_branch();
        for (x, v) in iter.into_iter() {
            t.insert_by_path(x.into_trie_key(), v);
        }
        t
    }
}

/// NK for Node Key;
/// TP for Trie Path;
/// TPR for Trie Path Ref;
impl<K, V, NK> Trie<K, V, NK>
where
    NK: Hash + Eq,
{
    fn create<TP>(mut k: TP, v: V) -> Self
    where
        TP: TriePath<NK>,
    {
        if let Some(x) = k.next() {
            let t = Self::create(k, v);
            let mut map = HashMap::new();
            map.insert(x, t);
            Self::Branch(None, map)
        } else {
            Self::Leaf(v)
        }
    }

    pub fn insert_by_path<TP>(&mut self, mut k: TP, v: V) -> Option<V>
    where
        TP: TriePath<NK>,
    {
        if let Some(x) = k.next() {
            // Make `self` a branch if it is not
            if let Trie::Leaf(v) = self {
                unsafe {
                    let v_value = std::ptr::read(v as *const V);
                    std::ptr::write(self as *mut _, Self::Branch(Some(v_value), HashMap::new()));
                }
            }
            match self {
                Trie::Branch(_, map) => {
                    if let Some(t) = map.get_mut(&x) {
                        t.insert_by_path(k, v)
                    } else {
                        let sub_t = Self::create(k, v);
                        map.insert(x, sub_t);
                        None
                    }
                }
                _ => unreachable!(),
            }
        } else {
            match self {
                Trie::Branch(Some(old_v), _) | Trie::Leaf(old_v) => {
                    let mut v = v;
                    std::mem::swap(old_v, &mut v);
                    Some(v)
                }
                Trie::Branch(none, _) => {
                    *none = Some(v);
                    None
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn get_by_path<'k, TPR>(&self, mut k: TPR) -> Option<&V>
    where
        TPR: TriePathRef<'k, NK>,
        NK: 'k,
    {
        if let Some(x) = k.next() {
            match self {
                Trie::Leaf(_) => None,
                Trie::Branch(_, map) => {
                    let t = map.get(x)?;
                    t.get_by_path(k)
                }
                _ => unreachable!(),
            }
        } else {
            match self {
                Trie::Leaf(v) => Some(v),
                Trie::Branch(Some(v), _) => Some(v),
                Trie::Branch(None, _) => None,
                _ => unreachable!(),
            }
        }
    }

    pub fn get_mut_by_path<'k, TPR>(&mut self, mut k: TPR) -> Option<&mut V>
    where
        TPR: TriePathRef<'k, NK>,
        NK: 'k,
    {
        if let Some(x) = k.next() {
            match self {
                Trie::Leaf(_) => None,
                Trie::Branch(_, map) => {
                    let t = map.get_mut(x)?;
                    t.get_mut_by_path(k)
                }
                _ => unreachable!(),
            }
        } else {
            match self {
                Trie::Leaf(v) => Some(v),
                Trie::Branch(Some(v), _) => Some(v),
                Trie::Branch(None, _) => None,
                _ => unreachable!(),
            }
        }
    }

    pub fn contains_path<'k, TPR>(&self, x: TPR) -> bool
    where
        TPR: TriePathRef<'k, NK>,
        NK: 'k,
    {
        self.get_by_path(x).is_some()
    }

    pub fn contains_key<'k>(&self, x: &K) -> bool
    where
        K: TrieKey<NK>,
        NK: 'k,
    {
        self.get_by_path(x.trie_key_ref()).is_some()
    }

    pub fn insert(&mut self, x: K, v: V) -> Option<V>
    where
        K: TrieKey<NK>,
    {
        self.insert_by_path(x.into_trie_key(), v)
    }

    pub fn get<'k>(&self, x: &K) -> Option<&V>
    where
        K: TrieKey<NK>,
        NK: 'k,
    {
        self.get_by_path(x.trie_key_ref())
    }

    pub fn get_mut<'k>(&mut self, x: &K) -> Option<&mut V>
    where
        K: TrieKey<NK>,
        NK: 'k,
    {
        self.get_mut_by_path(x.trie_key_ref())
    }

    pub fn iter_with_path<'a>(&'a self, p: K) -> Box<dyn Iterator<Item = (K, &V)> + 'a>
    where
        NK: Clone,
        K: ConstructTriePath<NK>,
    {
        match self {
            Self::Leaf(v) => Box::new([(p, v)].into_iter()),
            Self::Branch(v, map) => {
                let iter = map
                    .iter()
                    .map({
                        let p = p.clone();
                        move |(x, t)| t.iter_with_path(p.clone().pushed(x.clone()))
                    })
                    .flatten();
                if let Some(v) = v {
                    Box::new(iter.chain([(p, v)]))
                } else {
                    Box::new(iter)
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, &V)>
    where
        NK: Clone,
        K: ConstructTriePath<NK>,
    {
        self.iter_with_path(K::empty())
    }

    pub fn iter_mut_with_path<'a>(
        &'a mut self,
        p: K,
    ) -> Box<dyn Iterator<Item = (K, &mut V)> + 'a>
    where
        NK: Clone,
        K: ConstructTriePath<NK>,
    {
        match self {
            Self::Leaf(v) => Box::new([(p, v)].into_iter()),
            Self::Branch(v, map) => {
                let iter = map
                    .iter_mut()
                    .map({
                        let p = p.clone();
                        move |(x, t)| t.iter_mut_with_path(p.clone().pushed(x.clone()))
                    })
                    .flatten();
                if let Some(v) = v {
                    Box::new(iter.chain([(p, v)]))
                } else {
                    Box::new(iter)
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (K, &mut V)>
    where
        NK: Clone,
        K: ConstructTriePath<NK>,
    {
        self.iter_mut_with_path(K::empty())
    }

    pub fn into_iter_with_path<'a>(self, p: K) -> Box<dyn Iterator<Item = (K, V)> + 'a>
    where
        NK: Clone + 'a,
        V: 'a,
        K: ConstructTriePath<NK> + 'a,
    {
        match self {
            Self::Leaf(v) => Box::new([(p, v)].into_iter()),
            Self::Branch(v, map) => {
                let iter = map
                    .into_iter()
                    .map({
                        let p = p.clone();
                        move |(x, t)| t.into_iter_with_path(p.clone().pushed(x))
                    })
                    .flatten();
                if let Some(v) = v {
                    Box::new(iter.chain([(p, v)].into_iter()))
                } else {
                    Box::new(iter)
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn into_iter<'a>(self) -> Box<dyn Iterator<Item = (K, V)> + 'a>
    where
        NK: Clone + 'a,
        V: 'a,
        K: ConstructTriePath<NK> + 'a,
    {
        self.into_iter_with_path(K::empty())
    }
}

pub trait TrieKey<NK>
where
    NK: Hash + Eq,
{
    type TP: TriePath<NK>;
    fn trie_key_ref<'a>(&'a self) -> <Self::TP as TriePath<NK>>::TPR<'a>;
    fn into_trie_key(self) -> Self::TP;
}

pub trait ConstructTriePath<NK>: Clone {
    fn empty() -> Self;
    fn push(&mut self, k: NK);
    fn pushed(mut self, k: NK) -> Self {
        self.push(k);
        self
    }
}

pub trait TriePath<NK>: Iterator<Item = NK> + Clone
where
    NK: Hash + Eq,
{
    type TPR<'k>: TriePathRef<'k, NK>
    where
        NK: 'k,
        Self: 'k;
    fn as_ref<'k>(&'k self) -> Self::TPR<'k>;
}
pub trait TriePathRef<'k, K>: Iterator<Item = &'k K> + Clone
where
    K: Hash + Eq + 'k,
{
}

#[derive(Clone, Copy)]
pub struct PathRef<'r, 's>(&'r [&'s str]);

impl<'r, 's> PathRef<'r, 's> {
    pub fn iter(&self) -> PathRefIter<'r, 's> {
        PathRefIter { r: *self, i: 0 }
    }
}

#[derive(Clone)]
pub struct PathIter<'s> {
    r: Path<'s>,
    i: usize,
}

impl<'s> Iterator for PathIter<'s> {
    type Item = &'s str;
    fn next(&mut self) -> Option<Self::Item> {
        if self.i < self.r.0.len() {
            self.i += 1;
            Some(self.r.0[self.i - 1])
        } else {
            None
        }
    }
}

#[derive(Clone, Copy)]
pub struct PathRefIter<'r, 's> {
    r: PathRef<'r, 's>,
    i: usize,
}

impl<'r, 's> Iterator for PathRefIter<'r, 's> {
    type Item = &'r &'s str;
    fn next(&mut self) -> Option<Self::Item> {
        if self.i < self.r.0.len() {
            self.i += 1;
            Some(&self.r.0[self.i - 1])
        } else {
            None
        }
    }
}

impl<'s, 'k> TriePathRef<'k, &'s str> for PathRefIter<'k, 's> where 's: 'k {}

impl<'s> TriePath<&'s str> for PathIter<'s> {
    type TPR<'k> = PathRefIter<'k, 's> where 's: 'k, Self: 'k;
    fn as_ref<'k>(&'k self) -> PathRefIter<'k, 's> {
        PathRefIter {
            r: self.r.as_ref(),
            i: self.i,
        }
    }
}

impl<'s> Symbols<'s> {
    pub fn new() -> Self {
        Self {
            functions: FunctionIdTable::new(),
            constructors: Trie::new_branch(),
            datas: Trie::new_branch(),
            builtins: Trie::new_branch(),
            relations: Trie::new_branch(),
            instances: InstanceIdTable::new(),
            methods: Trie::new_branch(),
        }
    }
}

pub struct ExprHeap<'s>(Vec<Expr<'s>>);

impl<'s> std::ops::Index<ExprId> for ExprHeap<'s> {
    type Output = Expr<'s>;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.0[index.0]
    }
}

impl<'s> std::ops::IndexMut<ExprId> for ExprHeap<'s> {
    fn index_mut(&mut self, index: ExprId) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}

impl<'s> ExprHeap<'s> {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn push(&mut self, node: Expr<'s>) -> ExprId {
        let id = self.0.len();
        self.0.push(node);
        id.into()
    }
    pub fn push_many(&mut self, exprs: impl Iterator<Item = Expr<'s>>) -> Vec<ExprId> {
        exprs.map(|e| self.push(e)).collect()
    }
}
