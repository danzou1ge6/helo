use std::collections::HashMap;

pub use helo_parse::typed::Tag;

pub mod optimizations;

#[derive(Debug, Clone, Copy, Hash)]
pub struct ExprId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub usize);

impl std::fmt::Display for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

impl PartialEq for ExprId {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl LocalId {
    pub fn offset(self, x: usize) -> Self {
        Self(self.0 + x)
    }
}

impl From<usize> for LocalId {
    fn from(value: usize) -> Self {
        Self(value)
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

#[derive(Clone)]
pub struct Function {
    pub local_cnt: usize,
    pub arity: usize,
    pub capture_cnt: usize,
    pub body: ExprId,
    pub meta: helo_parse::ast::Meta,
    pub name: StrId,
    pub has_return: bool,
}

use helo_parse::{ast, inferer};

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

use builtins::BUILTIN_NAMES;
use helo_runtime::builtins;
pub use helo_runtime::builtins::BuiltinId;

pub struct BuiltinTable<'s> {
    tab: HashMap<ast::BuiltinFunctionName<'s>, BuiltinId>,
}

impl<'s> BuiltinTable<'s> {
    pub fn new() -> Self {
        let mut tab = HashMap::new();
        for (i, (mod_str, ident, _)) in BUILTIN_NAMES.iter().enumerate() {
            let mod_path = ast::Path::parse_simple(&mod_str);
            let path = mod_path.pushed(&ident);
            tab.insert(ast::BuiltinFunctionName(path), BuiltinId(i as u16));
        }
        Self { tab }
    }
    pub fn id_by_name(&self, name: &ast::BuiltinFunctionName<'_>) -> BuiltinId {
        *self.tab.get(name).unwrap()
    }
    pub fn arity_by_name(&self, name: &ast::BuiltinFunctionName<'_>) -> usize {
        let id = self.tab.get(name).unwrap();
        builtins::get_arity(*id)
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

pub use ast::CapturedId;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionId<'s> {
    pub ast_id: ast::FunctionId<'s>,
    pub type_args: inferer::TypeVarStore<'s>,
}

impl<'s> FunctionId<'s> {
    pub fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ins_tab: &ast::InstanceTable<'s>,
    ) -> std::fmt::Result {
        self.ast_id.fmt(f, ins_tab)?;
        write!(f, ".[")?;
        ast::str_join_vec(f, ", ", self.type_args.as_slice())?;
        write!(f, "]")
    }
    pub fn to_string(&self, ins_tab: &ast::InstanceTable<'s>) -> String {
        let mut buf = String::new();
        let mut formatter = core::fmt::Formatter::new(&mut buf);
        self.fmt(&mut formatter, ins_tab).unwrap();
        buf
    }
}

impl<'s> FunctionId<'s> {
    pub fn of_non_generic(id: ast::FunctionId<'s>) -> Self {
        Self {
            ast_id: id,
            type_args: inferer::TypeVarStore::empty(),
        }
    }
}

pub struct FunctionTable<'s> {
    tab: HashMap<FunctionId<'s>, Option<Function>>,
}

impl<'s> FunctionTable<'s> {
    pub fn new() -> Self {
        Self {
            tab: HashMap::new(),
        }
    }
    pub fn function_ids(&self) -> impl Iterator<Item = &FunctionId<'s>> {
        self.tab.keys()
    }
    pub fn get(&self, id: &FunctionId<'s>) -> Option<&Function> {
        self.tab.get(id).map(|x| x.as_ref().unwrap())
    }
    pub fn reserve(&mut self, k: FunctionId<'s>) {
        self.tab.insert(k, None);
    }
    pub fn reserved(&mut self, k: &FunctionId<'s>) -> bool {
        self.tab.get(k).is_some_and(|v| v.is_none())
    }
    pub fn insert(&mut self, k: FunctionId<'s>, v: Function) {
        self.tab.insert(k, Some(v));
    }
    pub fn iter(&self) -> impl Iterator<Item = (&FunctionId<'s>, &Function)> {
        self.tab.iter().map(|(id, f)| (id, f.as_ref().unwrap()))
    }
}

#[derive(Debug, Clone)]
pub enum ExprNode<'s> {
    Never,
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
    Apply {
        callee: ExprId,
        args: Vec<ExprId>,
        callee_impure: bool,
    },
    Immediate(Immediate),
    MakeClosure(FunctionId<'s>, Vec<LocalId>),
    Local(LocalId),
    /// Indiacates that at .0 lies the closure being called. Distinguishes from [`ExprNode::Local`] for tail call optimization
    ThisClosure(LocalId),
    UserFunction(FunctionId<'s>),
    Builtin(ast::BuiltinFunctionName<'s>),
    VariantField(LocalId, usize),
    TupleField(LocalId, usize),
    MakeTuple(Vec<ExprId>),
    MakeTagged(Tag<'s>, Vec<ExprId>),
    Panic {
        file: StrId,
        span: (usize, usize),
        msg: StrId,
    },
    Assign(LocalId, ExprId),
    Seq(Vec<ExprId>, Option<ExprId>),
    If {
        test: ExprId,
        then: ExprId,
    },
    While {
        test: ExprId,
        then: ExprId,
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
pub struct Expr<'s>(pub ExprNode<'s>, Meta);

impl<'s> Expr<'s> {
    pub fn new(node: ExprNode<'s>, meta: Meta) -> Self {
        Self(node, meta)
    }
    pub fn node(&self) -> &ExprNode<'s> {
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

impl<'s> std::ops::IndexMut<ExprId> for ExprHeap<'s> {
    fn index_mut(&mut self, index: ExprId) -> &mut Self::Output {
        &mut self.0[index.0]
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
    pub fn walk(&self, i: ExprId, f: &mut impl FnMut(&Expr<'s>)) {
        f(&self[i]);

        use ExprNode::*;
        match self[i].node() {
            LetBind { value, in_, .. } => {
                self.walk(*value, f);
                self.walk(*in_, f);
            }
            SwitchTag(value, arms, default) => {
                self.walk(*value, f);
                arms.iter().for_each(|(_, e)| self.walk(*e, f));
                self.walk(*default, f)
            }
            Switch(value, arms, default) => {
                self.walk(*value, f);
                arms.iter().for_each(|(_, e)| self.walk(*e, f));
                self.walk(*default, f)
            }
            Cond(arms, default) => {
                arms.iter().for_each(|(e1, e2)| {
                    self.walk(*e1, f);
                    self.walk(*e2, f);
                });
                self.walk(*default, f);
            }
            IfElse { test, then, else_ } => {
                self.walk(*test, f);
                self.walk(*then, f);
                self.walk(*else_, f);
            }
            Apply { callee, args, .. } => {
                self.walk(*callee, f);
                args.iter().for_each(|e| self.walk(*e, f));
            }
            MakeTagged(_, v) | MakeTuple(v) => v.iter().for_each(|e| self.walk(*e, f)),
            Assign(_, e) => {
                self.walk(*e, f);
            }
            Seq(v, e) => {
                v.iter().for_each(|e| self.walk(*e, f));
                e.map(|e| self.walk(e, f));
            }
            If { test, then } => {
                self.walk(*test, f);
                self.walk(*then, f);
            }
            While { test, then } => {
                self.walk(*test, f);
                self.walk(*then, f);
            }
            _ => {}
        }
    }

    fn walk_helper(
        &mut self,
        i: ExprId,
        selector: &impl Fn(ExprId, &Self) -> bool,
        f: &mut impl FnMut(ExprId, &mut Self) -> ExprId,
    ) -> ExprNode<'s> {
        use ExprNode::*;
        let node = match self[i].node().clone() {
            LetBind { value, in_, local } => LetBind {
                local,
                value: self.apply(value, selector, f),
                in_: self.apply(in_, selector, f),
            },
            SwitchTag(value, arms, default) => SwitchTag(
                self.apply(value, selector, f),
                arms.into_iter()
                    .map(|(t, e)| (t.clone(), self.apply(e, selector, f)))
                    .collect(),
                self.apply(default, selector, f),
            ),
            Switch(value, arms, default) => Switch(
                self.apply(value, selector, f),
                arms.into_iter()
                    .map(|(t, e)| (t.clone(), self.apply(e, selector, f)))
                    .collect(),
                self.apply(default, selector, f),
            ),
            Cond(arms, default) => Cond(
                arms.into_iter()
                    .map(|(e1, e2)| (self.apply(e1, selector, f), self.apply(e2, selector, f)))
                    .collect(),
                self.apply(default, selector, f),
            ),
            IfElse { test, then, else_ } => IfElse {
                test: self.apply(test, selector, f),
                then: self.apply(then, selector, f),
                else_: self.apply(else_, selector, f),
            },
            Apply {
                callee,
                args,
                callee_impure,
            } => Apply {
                callee: self.apply(callee, selector, f),
                args: args
                    .into_iter()
                    .map(|arg| self.apply(arg, selector, f))
                    .collect(),
                callee_impure,
            },
            MakeTuple(v) => MakeTuple(v.into_iter().map(|e| self.apply(e, selector, f)).collect()),
            MakeTagged(t, v) => MakeTagged(
                t.clone(),
                v.into_iter().map(|e| self.apply(e, selector, f)).collect(),
            ),
            Assign(v, e) => Assign(v, self.apply(e, selector, f)),
            Seq(v, e) => Seq(
                v.into_iter().map(|e| self.apply(e, selector, f)).collect(),
                e.map(|e| self.apply(e, selector, f)),
            ),
            If { test, then } => If {
                test: self.apply(test, selector, f),
                then: self.apply(then, selector, f),
            },
            While { test, then } => While {
                test: self.apply(test, selector, f),
                then: self.apply(then, selector, f),
            },
            other => other.clone(),
        };
        node
    }

    /// Walk the tree in pre-order and returns a NEW tree.
    /// Nodes selected by `selector` are replaced by result of calling `f`, and ancestors of
    /// replaced nodes are cloned on heap
    pub fn apply(
        &mut self,
        i: ExprId,
        selector: &impl Fn(ExprId, &Self) -> bool,
        f: &mut impl FnMut(ExprId, &mut Self) -> ExprId,
    ) -> ExprId {
        if selector(i, &self) {
            return f(i, self);
        }

        let node = self.walk_helper(i, selector, f);

        let meta = self[i].meta().clone();
        self.push(Expr::new(node, meta))
    }

    /// Walk the tree in pre-order and return a INPLACE MODIFIED tree.
    /// Nodes selected by `selector` are replaced by aresult of calling `f`, but their ancestors
    /// are not cloned.
    pub fn apply_in_place(
        &mut self,
        i: ExprId,
        selector: &impl Fn(ExprId, &Self) -> bool,
        f: &mut impl FnMut(ExprId, &mut Self) -> ExprId,
    ) -> ExprId {
        if selector(i, &self) {
            return f(i, self);
        }

        let node = self.walk_helper(i, selector, f);

        let meta = self[i].meta().clone();
        self[i] = Expr::new(node, meta);
        i
    }
}
