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

impl Immediate {
    pub fn to_string(&self, str_list: &StrList) -> String {
        use Immediate::*;
        match self {
            Int(i) => i.to_string(),
            Float(f) => f.to_string(),
            Bool(b) => if *b { "true" } else { "false" }.to_string(),
            Str(id) => str_list.get(*id).to_string(),
        }
    }
}

pub struct StrTable {
    cnt: usize,
    tab: HashMap<String, StrId>,
}

impl StrTable {
    pub fn new() -> Self {
        Self {
            cnt: 0,
            tab: HashMap::new(),
        }
    }
    pub fn add(&mut self, s: String) -> StrId {
        *self.tab.entry(s).or_insert_with(|| {
            let id = StrId(self.cnt);
            self.cnt += 1;
            id
        })
    }
    pub fn to_list(self) -> StrList {
        let mut v = Vec::new();
        v.resize(self.cnt, String::new());
        StrList {
            v: self.tab.into_iter().fold(v, |mut accu, (s, id)| {
                accu[id.0] = s;
                accu
            }),
        }
    }
    pub fn immediate_from_constant(&mut self, c: ast::Constant<'_>) -> Immediate {
        match c {
            ast::Constant::Int(i) => Immediate::Int(i),
            ast::Constant::Float(i) => Immediate::Float(i),
            ast::Constant::Bool(i) => Immediate::Bool(i),
            ast::Constant::Str(s) => Immediate::Str(self.add(s.to_string())),
        }
    }
}

pub struct StrList {
    v: Vec<String>,
}

impl StrList {
    pub fn get(&self, id: StrId) -> &str {
        &self.v[id.0]
    }
}

pub use ast::{CapturedId, FunctionId};

pub struct FunctionTable {
    tab: HashMap<FunctionId, Function>,
}

impl FunctionTable {
    pub fn new() -> Self {
        Self {
            tab: HashMap::new(),
        }
    }
    pub fn function_names(&self) -> impl Iterator<Item = &FunctionId> {
        self.tab.keys()
    }
    pub fn get(&self, name: &str) -> Option<&Function> {
        self.tab.get(name)
    }
    pub fn insert(&mut self, k: FunctionId, v: Function) {
        self.tab.insert(k, v);
    }
}

#[derive(Debug, Clone)]
pub enum ExprNode<'s> {
    LetBind {
        local: LocalId,
        value: ExprId,
        in_: ExprId,
    },
    SwitchTag(ExprId, Vec<(Tag<'s>, ExprId)>, ExprId),
    Switch(ExprId, Vec<(Immediate, ExprId)>, ExprId),
    Cond(Vec<(ExprId, ExprId)>, ExprId),
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
    MakeTagged(Tag<'s>, Vec<ExprId>),
    ThisClosure(FunctionId),
    Panic(&'static str),
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
pub struct Expr<'s>(ExprNode<'s>, Meta);

impl<'s> Expr<'s> {
    pub fn new(node: ExprNode<'s>, meta: Meta) -> Self {
        Self(node, meta)
    }
    pub fn node(&self) -> &ExprNode {
        &self.0
    }
    pub fn meta(&self) -> &Meta {
        &self.1
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
    pub fn push(&mut self, node: Expr<'s>) -> ExprId {
        let id = self.0.len();
        self.0.push(node);
        id.into()
    }
    pub fn push_many(&mut self, exprs: impl Iterator<Item = Expr<'s>>) -> Vec<ExprId> {
        exprs.map(|e| self.push(e)).collect()
    }
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn get(&self, index: ExprId) -> Option<&Expr> {
        self.0.get(index.0)
    }
    pub fn get_mut(&mut self, index: ExprId) -> Option<&mut Expr<'s>> {
        self.0.get_mut(index.0)
    }
}
