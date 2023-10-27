use std::collections::HashMap;

pub use helo_parse::ast::LocalId;
pub use helo_parse::typed::Tag;

#[derive(Debug, Clone, Copy, Hash)]
pub struct ExprId(pub usize);

impl PartialEq for ExprId {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for ExprId {}

impl From<usize> for ExprId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StrId(pub usize);

impl std::fmt::Display for StrId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct Function {
    pub local_cnt: usize,
    pub arity: usize,
    pub body: ExprId,
    pub meta: helo_parse::ast::Meta,
    pub name: StrId,
}

use helo_parse::ast;

#[derive(Debug, Clone, Hash)]
pub enum Immediate {
    Int(i64),
    Float(String),
    Bool(bool),
    Str(StrId),
    Char(char),
}

impl Immediate {
    pub fn to_string(&self, str_list: &StrList) -> String {
        use Immediate::*;
        match self {
            Int(i) => i.to_string(),
            Float(f) => f.to_string(),
            Bool(b) => if *b { "true" } else { "false" }.to_string(),
            Str(id) => str_list.get(*id).to_string(),
            Char(c) => c.to_string(),
        }
    }
    pub fn unwrap_int(self) -> i64 {
        match self {
            Self::Int(i) => i,
            _ => panic!("called `unwrap_int` on Immediate that is {:?}", self),
        }
    }
    pub fn unwrap_float(self) -> String {
        match self {
            Self::Float(i) => i,
            _ => panic!("called `unwrap_float` on Immediate that is {:?}", self),
        }
    }
    pub fn unwrap_bool(self) -> bool {
        match self {
            Self::Bool(i) => i,
            _ => panic!("called `unwrap_bool` on Immediate that is {:?}", self),
        }
    }
    pub fn unwrap_str(self) -> StrId {
        match self {
            Self::Str(i) => i,
            _ => panic!("called `unwrap_str` on Immediate that is {:?}", self),
        }
    }
    pub fn unwrap_char(self) -> char {
        match self {
            Self::Char(i) => i,
            _ => panic!("called `unwrap_char` on Immediate that is {:?}", self),
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
    pub fn add_str(&mut self, s: &'static str) -> StrId {
        if self.tab.contains_key(s) {
            return *self.tab.get(s).unwrap();
        }
        let id = StrId(self.cnt);
        self.cnt += 1;
        self.tab.insert(s.to_string(), id);
        id
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
            ast::Constant::Float(i) => Immediate::Float(i.to_string()),
            ast::Constant::Bool(i) => Immediate::Bool(i),
            ast::Constant::Str(s) => Immediate::Str(self.add(s.to_string())),
            ast::Constant::Char(c) => Immediate::Char(c),
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
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.v.iter().map(|x| &x[..])
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
    /// Indiacates that at .0 lies the closure being called. Distinguishes from [`ExprNode::Local`] for tail call optimization
    ThisClosure(LocalId),
    UserFunction(FunctionId),
    Builtin(&'s str),
    VariantField(LocalId, usize),
    TupleField(LocalId, usize),
    MakeTuple(Vec<ExprId>),
    MakeTagged(Tag<'s>, Vec<ExprId>),
    Panic {
        file: StrId,
        span: (usize, usize),
        msg: StrId,
    },
}

impl<'s> ExprNode<'s> {
    pub fn panic(meta: &ast::Meta, msg: &'static str, str_table: &mut StrTable) -> Self {
        Self::Panic {
            file: str_table.add(meta.file_name.to_string()),
            msg: str_table.add_str(msg),
            span: meta.span,
        }
    }
    pub fn panic_string(meta: &ast::Meta, msg: String, str_table: &mut StrTable) -> Self {
        Self::Panic {
            file: str_table.add(meta.file_name.to_string()),
            msg: str_table.add(msg),
            span: meta.span,
        }
    }
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
