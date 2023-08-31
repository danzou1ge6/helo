#[derive(Debug, Clone, Copy)]
pub struct ExprId(usize);

impl From<usize> for ExprId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocalId(usize);

impl LocalId {
    pub fn type_var_id(&self) -> TypeVarId {
        TypeVarId(self.0)
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

#[derive(Debug)]
pub enum ExprNode_<'s, Id, C> {
    Call { callee: Id, args: Vec<Id> },
    IfElse { test: Id, then: Id, else_: Id },
    Case { operand: Id, arms: Vec<CaseArm<'s>> },
    LetIn { bind: LocalId, value: Id, in_: Id },
    Closure(C),
    Global(&'s str),
    Tuple(Vec<Id>),
    TupleGet(Id, usize),
    Constant(Constant<'s>),
    Local(LocalId),
}

pub type ExprNode<'s> = ExprNode_<'s, ExprId, Closure<'s>>;

#[derive(Debug, Clone)]
pub enum Constant<'s> {
    Int(i64),
    Float(f64),
    Str(&'s str),
    Bool(bool),
}

impl<'s> Constant<'s> {
    pub fn type_(&self) -> PrimitiveType {
        use Constant::*;
        match self {
            Int(_) => PrimitiveType::Int,
            Float(_) => PrimitiveType::Float,
            Bool(_) => PrimitiveType::Bool,
            Str(_) => PrimitiveType::Str,
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
            type_: None,
            meta,
        }
    }
}

/// Arguments supplied to `function` are laid out in the following way:
///     a_0, .. , a_n, (b_0, .. b_m)
/// where a_i 's are arguments to the closure, therefore n begin the arity of the closure; b_i 's are locals captured by
/// the closure
#[derive(Debug)]
pub struct Closure_<F> {
    pub captures: Vec<LocalId>,
    pub captures_meta: Vec<Meta>,
    pub function: F,
}

pub type Closure<'s> = Closure_<Function<'s>>;

#[derive(Debug)]
pub struct Function<'s> {
    pub type_: Option<CallableType<'s>>,
    pub var_cnt: usize,
    pub local_cnt: usize,
    pub arity: usize,
    pub body: ExprId,
    pub meta: Meta,
    pub param_metas: Vec<Meta>,
}

#[derive(Debug, Clone)]
pub struct CaseArm<'s> {
    pub pattern: Pattern<'s>,
    pub result: ExprId,
}

#[derive(Debug, Clone)]
pub enum Pattern<'s> {
    Construct(&'s str, Vec<Pattern<'s>>, Meta),
    Bind(LocalId, Meta),
    Literal(Constant<'s>, Meta),
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
        }
    }

    pub fn type_(
        &self,
        symbols: &Symbols<'s>,
        inferer: &mut crate::inferer::Inferer<'s>,
        e: &mut crate::errors::ManyError,
    ) -> Type<'s> {
        match self {
            Pattern::Bind(local_id, meta) => Type {
                node: TypeNode::Var(local_id.type_var_id()),
                meta: meta.clone(),
            },
            Pattern::Literal(c, meta) => Type {
                node: TypeNode::Primitive(c.type_()),
                meta: meta.clone(),
            },
            Pattern::Construct(constructor, args, meta) => {
                let args_type: Vec<_> = args.iter().map(|p| p.type_(symbols, inferer, e)).collect();
                let constructor = symbols.constructor(&constructor);
                let data = symbols.data(constructor.belongs_to);

                let type_var_zero = inferer.new_slots(data.kind_arity);
                let data_params = (0..data.kind_arity)
                    .map(|i| Type::new_var(type_var_zero.offset(i), data.generic_metas[i].clone()))
                    .collect();

                let params: Vec<_> = constructor
                    .params
                    .iter()
                    .map(|p| p.offset_vars(type_var_zero))
                    .collect();

                if let Err(err) = inferer.unify_list(args_type.iter(), params.iter(), meta) {
                    e.push(err);
                    return Type {
                        node: TypeNode::Never,
                        meta: meta.clone(),
                    };
                }
                Type {
                    node: TypeNode::Generic(constructor.belongs_to, data_params),
                    meta: meta.clone(),
                }
            }
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
use std::collections::HashMap;
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
    Generic(&'s str, Vec<Type<'s>>),
    Tuple(Vec<Type<'s>>),
    Primitive(PrimitiveType),
    Var(TypeVarId),
    UpperBounded(Box<Type<'s>>),
    Unit,
    Never,
}

impl<'s> std::fmt::Display for TypeNode<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TypeNode::*;
        match self {
            Callable(c) => {
                write!(f, "{c}")
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
            UpperBounded(v) => write!(f, "^{}", v),
            Unit => write!(f, "()"),
            Never => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallableType<'s> {
    pub params: Vec<Type<'s>>,
    pub ret: Box<Type<'s>>,
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

impl<'s> CallableType<'s> {
    pub fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> CallableType<'s> {
        let params = self.params.iter().map(|x| x.apply(selector, f)).collect();
        let ret = self.ret.apply(selector, f);
        CallableType {
            params,
            ret: Box::new(ret),
        }
    }

    pub fn substitute_vars_with_nodes(
        &self,
        nodes: &impl Fn(TypeVarId) -> TypeNode<'s>,
    ) -> CallableType<'s> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Int,
    Float,
    Str,
    Bool,
}

impl PrimitiveType {
    pub fn name(&self) -> &'static str {
        use PrimitiveType::*;
        match self {
            Int => "Int",
            Float => "Float",
            Str => "Str",
            Bool => "Bool",
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

impl<'s> Type<'s> {
    pub fn new_var(id: TypeVarId, meta: Meta) -> Self {
        Self {
            node: TypeNode::Var(id),
            meta,
        }
    }

    pub fn new_bounded_var(id: TypeVarId, meta: Meta) -> Self {
        Self {
            node: TypeNode::UpperBounded(Box::new(Self {
                node: TypeNode::Var(id),
                meta: meta.clone(),
            })),
            meta,
        }
    }
    /// Walk the tree, and apply `f` to any child that makes `selector` produce `true`
    pub fn apply(
        &self,
        selector: &impl Fn(&Type<'s>) -> bool,
        f: &mut impl FnMut(&Type<'s>) -> Type<'s>,
    ) -> Type<'s> {
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
            Tuple(v) => Tuple(apply_many(v.iter(), selector, f)),
            UpperBounded(v) => UpperBounded(Box::new(v.apply(selector, f))),
            Var(v) => Var(*v),
            Never => Never,
        };
        Type {
            node,
            meta: self.meta.clone(),
        }
    }

    pub fn walk(&self, f: &mut impl FnMut(&Type<'s>) -> ()) {
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

    /// Replace variable_i in the tree with `values[i]`
    /// WARN panic if `values[i]` goes out of bound!
    pub fn substitute_vars(&self, values: &[Type<'s>]) -> Type<'s> {
        self.apply(
            &|t| matches!(t.node, TypeNode::Var(_)),
            &mut |t| match t.node {
                TypeNode::Var(v) => values[v.0].clone(),
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
            labels = vec![
                miette::LabeledSpan::at(span, format!("{}", self.node))
            ],
            "Type here"
        ).with_source_code(self.meta.named_source());
        writeln!(f, "{:?}", report)
    }
}

pub struct BuiltinFunction<'s> {
    pub var_cnt: usize,
    pub type_: CallableType<'s>,
    pub meta: Meta,
}

pub struct Symbols<'s> {
    functions: HashMap<&'s str, Function<'s>>,
    constructors: HashMap<&'s str, Constructor<'s>>,
    datas: HashMap<&'s str, Data<'s>>,
    builtins: HashMap<&'s str, BuiltinFunction<'s>>,
}

impl<'s> Symbols<'s> {
    pub fn function_names(&self) -> impl Iterator<Item = &&'s str> {
        self.functions.keys()
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
    pub fn function(&self, name: &str) -> &Function<'s> {
        self.functions.get(name).unwrap()
    }
    pub fn get_function(&self, name: &str) -> Option<&Function<'s>> {
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
            Tuple(v) => {
                for elem in v {
                    self.validate_type(elem)?;
                }
                Ok(())
            }
            Primitive(_) | Var(_) | UpperBounded(_) | Unit | Never => Ok(()),
        }
    }
    pub fn validate_callable_type(&self, type_: &CallableType<'s>) -> Result<(), miette::Report> {
        for p in &type_.params {
            self.validate_type(p)?;
        }
        self.validate_type(&type_.ret)
    }

    pub fn get_builtin_type(&self, name: &str) -> Option<&BuiltinFunction<'s>> {
        self.builtins.get(name)
    }
}

impl<'s> Symbols<'s> {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            constructors: HashMap::new(),
            datas: HashMap::new(),
            builtins: HashMap::new(),
        }
    }
    pub fn add_function(&mut self, name: &'s str, f: Function<'s>) {
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
