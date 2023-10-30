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

pub type FunctionId = String;

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
    MakeClosure(FunctionId),
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
    Str(&'s str),
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
    pub type_: Option<Type<'s>>,
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
    pub fn new(node: ExprNode<'s>, meta: Meta, type_: Option<Type<'s>>) -> Self {
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
    Construct(&'s str, Vec<Pattern<'s>>, Meta),
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
    pub name: &'s str,
    pub params: Vec<Type<'s>>,
    pub belongs_to: &'s str,
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
    pub fn unwrap_construct(self) -> (&'s str, Vec<Pattern<'s>>, Meta) {
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
                if let Some(_) = symbols.get_constructor(&constructor) {
                    for a in args {
                        a.validate(symbols)?;
                    }
                    Ok(())
                } else {
                    Err(miette::Report::new(errors::ConstructorNotFound::new(
                        constructor,
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
                let c = symbols.constructor(&constructor);
                let data = symbols.data(c.belongs_to);
                data.constructors.len() == 1 && args.iter().all(|pat| pat.inrefutable(symbols))
            }
            Self::Tuple(elements, _) => elements.iter().all(|pat| pat.inrefutable(symbols)),
            Self::Literal(_, _) => true,
            Self::Bind(_, _) => true,
        }
    }
}

pub struct Data<'s> {
    pub name: &'s str,
    pub kind_arity: usize,
    pub constructors: Vec<&'s str>,
    pub meta: Meta,
    pub generic_metas: Vec<Meta>,
}

use std::borrow::Borrow;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
#[derive(Clone, Debug)]
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

#[derive(Debug, Clone)]
pub enum TypeNode<'s> {
    Callable(CallableType<'s>),
    ImpureCallable(CallableType<'s>),
    Generic(&'s str, Vec<Type<'s>>),
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
            Generic(template, args) => {
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

#[derive(Debug, Clone)]
pub struct CallableType<'s> {
    pub params: Vec<Type<'s>>,
    pub ret: Box<Type<'s>>,
}

#[derive(Clone, Debug)]
pub struct FunctionType<'s> {
    pub params: Vec<Type<'s>>,
    pub ret: Box<Type<'s>>,
    pub captures: Vec<Type<'s>>,
}

impl<'s> FunctionType<'s> {
    pub fn collect_vars(&self, vars: &mut HashMap<TypeVarId, Meta>) {
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

fn str_join_vec<T>(f: &mut std::fmt::Formatter<'_>, j: &str, slice: &[T]) -> std::fmt::Result
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
        write!(f, "][")?;
        str_join_vec(f, ", ", &self.captures)?;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Clone)]
pub struct Type<'s> {
    pub node: TypeNode<'s>,
    pub meta: Meta,
}

pub trait TypeApply<'s> {
    fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> Self;

    fn substitute_vars_with_nodes(&self, nodes: &impl Fn(TypeVarId) -> TypeNode<'s>) -> Self
    where
        Self: Sized,
    {
        self.apply(
            &|t| matches!(t.node, TypeNode::Var(_)),
            &mut |t| match t.node {
                TypeNode::Var(v) => Type {
                    node: nodes(v),
                    meta: t.meta.clone(),
                },
                _ => unreachable!(),
            },
        )
    }
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
            Generic(template, args) => Generic(template, apply_many(args.iter(), selector, f)),
            Callable(v) => Callable(CallableType {
                params: apply_many(v.params.iter(), selector, f),
                ret: Box::new(v.ret.apply(selector, f)),
            }),
            ImpureCallable(v) => Callable(CallableType {
                params: apply_many(v.params.iter(), selector, f),
                ret: Box::new(v.ret.apply(selector, f)),
            }),
            Tuple(v) => Tuple(apply_many(v.iter(), selector, f)),
            Var(v) => Var(*v),
            Never => Never,
            WildCard => WildCard,
        };
        Type {
            node,
            meta: self.meta.clone(),
        }
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
    pub fn is_unit(&self) -> bool {
        matches!(self, TypeNode::Unit)
    }
}

impl<'s> Type<'s> {
    pub fn new_var(id: TypeVarId, meta: Meta) -> Self {
        Self {
            node: TypeNode::Var(id),
            meta,
        }
    }

    pub fn new_never(meta: Meta) -> Self {
        Self {
            node: TypeNode::Never,
            meta,
        }
    }

    pub fn new_unit(meta: Meta) -> Self {
        Self {
            node: TypeNode::Unit,
            meta,
        }
    }

    pub fn apply_result<E>(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Result<Type<'s>, E>,
    ) -> Result<Type<'s>, E> {
        use TypeNode::*;
        fn apply_many<'a, 's, E>(
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
        if selector(self) {
            return f(self);
        }
        let node = match &self.node {
            Primitive(p) => Primitive(*p),
            Unit => Unit,
            Generic(template, args) => Generic(template, apply_many(args.iter(), selector, f)?),
            Callable(v) => Callable(CallableType {
                params: apply_many(v.params.iter(), selector, f)?,
                ret: Box::new(v.ret.apply_result(selector, f)?),
            }),
            ImpureCallable(v) => Callable(CallableType {
                params: apply_many(v.params.iter(), selector, f)?,
                ret: Box::new(v.ret.apply_result(selector, f)?),
            }),
            Tuple(v) => Tuple(apply_many(v.iter(), selector, f)?),
            Var(v) => Var(*v),
            Never => Never,
            WildCard => WildCard,
        };
        Ok(Type {
            node,
            meta: self.meta.clone(),
        })
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
            Callable(c) => {
                for p in &c.params {
                    p.walk(f);
                }
                c.ret.walk(f);
            }
            _ => {}
        }
    }

    pub fn collect_vars(&self, vars: &mut HashMap<TypeVarId, Meta>) {
        self.walk(&mut |t| match &t.node {
            TypeNode::Var(v) => {
                vars.insert(*v, t.meta.clone());
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
                    meta: t.meta.clone(),
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
        let span = (self.meta.span.0)..(self.meta.span.0 + self.meta.span.1);
        let report = miette::miette!(
            labels = vec![miette::LabeledSpan::at(span, format!("{}", self.node))],
            "Type here"
        )
        .with_source_code(self.meta.named_source());
        writeln!(f, "{:?}", report)
    }
}

pub struct BuiltinFunction<'s> {
    pub var_cnt: usize,
    pub type_: CallableType<'s>,
    pub meta: Meta,
    pub pure: bool,
}

pub struct Symbols_<'s, F> {
    pub(crate) functions: HashMap<FunctionId, F>,
    pub(crate) constructors: HashMap<&'s str, Constructor<'s>>,
    pub(crate) datas: HashMap<&'s str, Data<'s>>,
    pub(crate) builtins: HashMap<&'s str, BuiltinFunction<'s>>,
}

pub type Symbols<'s> = Symbols_<'s, Function<'s>>;

impl<'s, F> Symbols_<'s, F> {
    pub fn function_names(&self) -> impl Iterator<Item = &str> {
        self.functions.keys().map(|x| &x[..])
    }
    pub fn get_constructor(&self, name: &str) -> Option<&Constructor<'s>> {
        self.constructors.get(name)
    }
    pub fn constructor(&self, name: &str) -> &Constructor<'s> {
        self.constructors.get(name).unwrap()
    }
    pub fn get_data(&self, name: &str) -> Option<&Data<'s>> {
        self.datas.get(name)
    }
    pub fn data(&self, name: &str) -> &Data<'s> {
        self.datas.get(name).unwrap()
    }
    pub fn function(&self, name: &str) -> &F {
        self.functions.get(name).unwrap()
    }
    pub fn get_function(&self, name: &str) -> Option<&F> {
        self.functions.get(name)
    }

    pub fn validate_type(&self, type_: &Type<'s>) -> Result<(), miette::Report> {
        use crate::errors;
        use TypeNode::*;
        match &type_.node {
            Generic(template, args) => {
                if let None = self.get_data(&template) {
                    return Err(miette::Report::new(errors::ConstructorNotFound::new(
                        &template,
                        &type_.meta,
                    )));
                }
                if args.len() != self.data(&template).kind_arity {
                    return Err(miette::Report::new(errors::WrongNumberOfArgs::new(
                        self.data(&template).kind_arity,
                        &type_.meta,
                    )));
                }
                Ok(())
            }
            Callable(callable) => {
                for p in &callable.params {
                    self.validate_type(p)?;
                }
                self.validate_type(&callable.ret)?;
                Ok(())
            }
            ImpureCallable(callable) => {
                for p in &callable.params {
                    self.validate_type(p)?;
                }
                self.validate_type(&callable.ret)?;
                Ok(())
            }
            Tuple(v) => {
                for elem in v {
                    self.validate_type(elem)?;
                }
                Ok(())
            }
            Primitive(_) | Var(_) | Unit | Never | WildCard => Ok(()),
        }
    }
    pub fn validate_callable_type(&self, type_: &CallableType<'s>) -> Result<(), miette::Report> {
        for p in &type_.params {
            self.validate_type(p)?;
        }
        self.validate_type(&type_.ret)
    }

    pub fn get_builtin(&self, name: &str) -> Option<&BuiltinFunction<'s>> {
        self.builtins.get(name)
    }
}

impl<'s, F> Symbols_<'s, F> {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            constructors: HashMap::new(),
            datas: HashMap::new(),
            builtins: HashMap::new(),
        }
    }
    pub fn add_function(&mut self, name: FunctionId, f: F) {
        self.functions.insert(name, f);
    }
    pub fn add_constructor(&mut self, name: &'s str, c: Constructor<'s>) {
        self.constructors.insert(name, c);
    }
    pub fn add_data(&mut self, name: &'s str, d: Data<'s>) {
        self.datas.insert(name, d);
    }
    pub fn add_builtin(&mut self, name: &'s str, f: BuiltinFunction<'s>) {
        self.builtins.insert(name, f);
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
