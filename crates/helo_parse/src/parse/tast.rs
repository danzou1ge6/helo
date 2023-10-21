use crate::ast;

#[derive(Clone, Debug)]
pub struct CaseArm<'s> {
    pub pattern: Pattern<'s>,
    pub guard: Option<Expr<'s>>,
    pub result: Expr<'s>,
}

pub use ast::{Constant, Meta};

#[derive(Clone, Debug)]
pub enum Pattern<'s> {
    Construct(&'s str, Vec<Pattern<'s>>, Meta),
    Bind(&'s str, Meta),
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
    Constant(ast::Constant<'s>),
    Identifier(&'s str),
}

#[derive(Debug, Clone)]
pub struct Function<'s> {
    pub type_: Option<ast::CallableType<'s>>,
    pub var_cnt: usize,
    pub params: Vec<&'s str>,
    pub param_metas: Vec<ast::Meta>,
    pub body: Box<Expr<'s>>,
    pub meta: ast::Meta,
}

use ast::Symbols_;

pub type Symbols<'s> = Symbols_<'s, Function<'s>>;
