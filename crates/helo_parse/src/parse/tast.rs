use crate::ast;

#[derive(Clone, Debug)]
pub struct CaseArm<'s> {
    pub pattern: Pattern<'s>,
    pub guard: Option<Expr<'s>>,
    pub result: Expr<'s>,
}

pub use ast::Meta;

#[derive(Clone, Debug)]
pub enum Pattern<'s> {
    Construct(&'s str, Vec<Pattern<'s>>, Meta),
    Bind(&'s str, bool, Meta),
    Literal(Constant<'s>, Meta),
    Tuple(Vec<Pattern<'s>>, Meta),
}
pub use ast::{CallableType, Constructor, Type, TypeNode};

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
    pub type_: Option<ast::CallableType<'s>>,
    pub var_cnt: usize,
    pub params: Vec<&'s str>,
    pub param_metas: Vec<ast::Meta>,
    pub body: Box<Expr<'s>>,
    pub meta: ast::Meta,
    pub pure: bool,
}

use ast::Symbols_;

pub type Symbols<'s> = Symbols_<'s, Function<'s>>;
