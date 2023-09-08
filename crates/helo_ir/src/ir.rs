use std::collections::HashMap;

pub use helo_parse::ast::LocalId;
pub use helo_parse::typed::Tag;

#[derive(Debug, Clone, Copy)]
pub struct ExprId(pub usize);

impl From<usize> for ExprId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StrId(pub usize);

pub struct Function {
    pub local_cnt: usize,
    pub arity: usize,
    pub body: ExprId,
}

use helo_parse::ast;

#[derive(Debug, Clone)]
pub enum Immediate {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(StrId),
}

pub struct StrTable {
    cnt: usize,
    tab: HashMap<String, StrId>,
}

impl StrTable {
    pub fn add(&mut self, s: String) -> StrId {
        *self.tab.entry(s).or_insert_with(|| {
            let id = StrId(self.cnt);
            self.cnt += 1;
            id
        })
    }
}

pub use ast::{CapturedId, FunctionId};

pub struct FunctionTable {
    tab: HashMap<FunctionId, Function>,
}

impl FunctionTable {
    pub fn insert(&mut self, k: FunctionId, v: Function) {
        self.tab.insert(k, v);
    }
}

#[derive(Debug, Clone)]
pub enum ExprNode {
    LetBind {
        local: LocalId,
        value: ExprId,
        in_: ExprId,
    },
    SwitchTag(ExprId, Vec<(Tag, ExprId)>, ExprId),
    Switch(ExprId, Vec<(Immediate, ExprId)>, ExprId),
    Cond(Vec<(ExprId, ExprId)>, Option<ExprId>),
    IfElse {
        test: ExprId,
        then: ExprId,
        else_: ExprId,
    },
    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },
    Immediate(Immediate),
    MakeClosure(FunctionId, Vec<LocalId>),
    Local(LocalId),
    UserFunction(FunctionId),
    Builtin(FunctionId),
    VariantField(LocalId, usize),
    TupleField(LocalId, usize),
    MakeTuple(Vec<ExprId>),
    MakeTagged(Tag, Vec<ExprId>),
    ThisClosure(FunctionId),
}

#[derive(Debug, Clone)]
pub struct Meta {
    pub file_name: std::sync::Arc<String>,
    pub span: (usize, usize),
}

impl From<&helo_parse::ast::Meta> for Meta {
    fn from(value: &helo_parse::ast::Meta) -> Self {
        Self {
            file_name: value.file_name.clone(),
            span: value.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr(ExprNode, Meta);

impl Expr {
    pub fn new(node: ExprNode, meta: Meta) -> Self {
        Self(node, meta)
    }
}

pub struct ExprHeap(Vec<Expr>);

impl std::ops::Index<ExprId> for ExprHeap {
    type Output = Expr;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.0[index.0]
    }
}

impl ExprHeap {
    pub fn push(&mut self, node: Expr) -> ExprId {
        let id = self.0.len();
        self.0.push(node);
        id.into()
    }
    pub fn push_many(&mut self, exprs: impl Iterator<Item = Expr>) -> Vec<ExprId> {
        exprs.map(|e| self.push(e)).collect()
    }
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn get(&self, index: ExprId) -> Option<&Expr> {
        self.0.get(index.0)
    }
    pub fn get_mut(&mut self, index: ExprId) -> Option<&mut Expr> {
        self.0.get_mut(index.0)
    }
}
