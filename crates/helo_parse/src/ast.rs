#[derive(Debug, Clone, Copy)]
pub struct ExprId(usize);

impl From<usize> for ExprId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

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

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct FunctionName<'s>(pub &'s str);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum FunctionId<'s> {
    Method(InstanceId<'s>, &'s str),
    Standard(&'s str),
    Closure {
        span: (usize, usize),
        file: Arc<String>,
    },
}

impl<'s> FunctionId<'s> {
    pub fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ins_tab: &InstanceTable<'s>,
    ) -> std::fmt::Result {
        match self {
            Self::Method(ins, name) => {
                write!(f, "{}<", ins.rel_name.0)?;
                let ins = ins_tab.get(ins).unwrap();
                str_join_vec(f, ", ", &ins.rel.args)?;
                write!(f, ">::{}", name)
            }
            Self::Standard(name) => write!(f, "{}", name),
            Self::Closure { span, file } => write!(f, "closure@{}:{}-{}", file, span.0, span.1),
        }
    }

    pub fn to_string(&self, ins_tab: &InstanceTable<'s>) -> String {
        let mut buf = String::new();
        let mut formatter = core::fmt::Formatter::new(&mut buf);
        self.fmt(&mut formatter, ins_tab).unwrap();
        buf
    }

    pub fn main() -> Self {
        Self::Standard("main")
    }
}

impl<'s> FunctionId<'s> {
    pub fn assume_exists(name: &'s str) -> Self {
        Self::Standard(name)
    }
    pub fn of_method(ins: InstanceId<'s>, name: FunctionName<'s>) -> Self {
        Self::Method(ins, name.0)
    }
    pub fn of_closure_at(meta: &Meta) -> Self {
        Self::Closure {
            span: meta.span,
            file: meta.file_name.clone(),
        }
    }
}

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
    Global(&'s str),
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
            Construct(ConstructorName(name), args, _) => {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstructorName<'s>(pub &'s str);

pub struct Constructor<'s> {
    pub name: &'s str,
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
                        *constructor,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DataName<'s>(pub &'s str);

pub struct Data<'s> {
    pub name: &'s str,
    pub kind_arity: usize,
    pub constructors: Vec<ConstructorName<'s>>,
    pub meta: Meta,
    pub generic_metas: Vec<Meta>,
}

use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
#[derive(Clone, Debug, Hash)]
pub struct Meta {
    pub span: (usize, usize),
    pub file_name: Arc<String>,
    pub src: Arc<String>,
}
use miette;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RelationName<'s>(pub &'s str);

#[derive(Clone, Debug)]
pub struct Relation<'s> {
    pub name: RelationName<'s>,
    /// Indicates whether some parameter is dependent on others
    pub dependent: Vec<usize>,
    pub constrains: Vec<Constrain<'s>>,
    pub arity: usize,
    pub f_sigs: HashMap<FunctionName<'s>, MethodSig<'s>>,
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
        write!(f, "{} ", self.rel_name.0)?;
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
            rel_name: self.rel_name,
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
            rel_name: self.rel_name,
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
    pub fn rel_name(&self) -> RelationName<'s> {
        self.rel.rel_name
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
            rel_name: self.name,
            args: (0..self.arity)
                .map(|x| Type::new_var(TypeVarId(x)))
                .collect(),
            meta: self.meta.clone(),
        }
    }
    pub fn method_sig(&self, fid: &FunctionName<'s>) -> &MethodSig<'s> {
        &self.f_sigs[fid]
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
            Generic(template, args) => Generic(*template, apply_many(args.iter(), selector, f)),
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
            _ => panic!("Only callable types have the notion of purity"),
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
                *template,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BuiltinFunctionName<'s>(pub &'s str);

impl<'s> std::fmt::Display for BuiltinFunctionName<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct BuiltinFunction<'s> {
    pub var_cnt: usize,
    pub type_: CallableType<'s>,
    pub meta: Meta,
    pub pure: bool,
}

pub struct InstanceTable<'s>(HashMap<RelationName<'s>, Vec<Instance<'s>>>);

impl<'s> InstanceTable<'s> {
    pub fn get<'a, 'b>(&'a self, id: &'b InstanceId<'s>) -> Option<&'a Instance<'s>> {
        let v = self.0.get(&id.rel_name)?;
        v.get(id.n)
    }
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn of_rel(&self, rel_name: &RelationName<'s>) -> &[Instance<'s>] {
        &self.0[rel_name]
    }
    pub fn insert(&mut self, rel_name: RelationName<'s>, instance: Instance<'s>) -> InstanceId<'s> {
        if self.0.contains_key(&rel_name) {
            let n = self.0.get(&rel_name).unwrap().len();
            self.0.get_mut(&rel_name).unwrap().push(instance);
            InstanceId { rel_name, n }
        } else {
            self.0.insert(rel_name, vec![instance]);
            InstanceId { rel_name, n: 0 }
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = (InstanceId<'s>, &Instance<'s>)> {
        self.0
            .iter()
            .map(|(name, v)| {
                v.iter()
                    .enumerate()
                    .map(|(n, ins)| (InstanceId { rel_name: *name, n }, ins))
            })
            .flatten()
    }
}

pub struct Symbols_<'s, F> {
    pub functions: HashMap<FunctionId<'s>, F>,
    pub constructors: HashMap<ConstructorName<'s>, Constructor<'s>>,
    pub datas: HashMap<DataName<'s>, Data<'s>>,
    pub builtins: HashMap<BuiltinFunctionName<'s>, BuiltinFunction<'s>>,
    pub relations: HashMap<RelationName<'s>, Relation<'s>>,
    pub instances: InstanceTable<'s>,
    pub methods: HashMap<FunctionName<'s>, RelationName<'s>>,
}

pub type Symbols<'s> = Symbols_<'s, Function<'s>>;

impl<'s, F> Symbols_<'s, F> {
    pub fn validate_type(&self, type_: &Type<'s>, meta: &Meta) -> Result<(), miette::Report> {
        use crate::errors;
        use TypeNode::*;
        match &type_.node {
            Generic(template, args) => {
                if let None = self.datas.get(&template) {
                    return Err(miette::Report::new(errors::DataNotFound::new(
                        *template, meta,
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

        let rel_arity = self.relations[&c.rel_name].arity;
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

impl<'s, F> Symbols_<'s, F> {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            constructors: HashMap::new(),
            datas: HashMap::new(),
            builtins: HashMap::new(),
            relations: HashMap::new(),
            instances: InstanceTable::new(),
            methods: HashMap::new(),
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
