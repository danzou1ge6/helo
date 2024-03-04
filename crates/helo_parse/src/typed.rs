use std::collections::HashMap;

use crate::ast::{self, DataName, RelationName};

use ast::{
    BuiltinFunctionName, CapturedId, Constant, ConstructorName, FunctionId, FunctionName, LocalId,
    Pattern,
};

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
    MakeClosure(FunctionId<'s>, ast::FunctionType<'s>),
    Constructor(ConstructorName<'s>),
    UserFunction(FunctionId<'s>),
    UnresolvedMethod {
        rel_name: RelationName<'s>,
        f_name: FunctionName<'s>,
        constrains: Vec<ast::Constrain<'s>>,
        primary_constrain: ast::Constrain<'s>,
    },
    Builtin(BuiltinFunctionName<'s>),
    Tuple(Vec<ExprId>),
    Captured {
        id: CapturedId,
        is_self: bool,
    },
    Constant(Constant<'s>),
    Local(LocalId),
    Seq(Vec<Stmt>, Option<ExprId>),
    AssignLocal(LocalId, ExprId),
    AssignCaptured(CapturedId, ExprId),
    Never,
}

#[derive(Debug, Clone)]
pub enum StmtNode {
    If { test: ExprId, then: ExprId },
    While { test: ExprId, then: ExprId },
    Expr(ExprId),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub node: StmtNode,
    pub meta: ast::Meta,
}

impl Stmt {
    pub fn walk<'s>(&self, expr_heap: &mut ExprHeap<'s>, f: &mut impl FnMut(&mut Expr<'s>)) {
        match &self.node {
            StmtNode::If { test, then } => {
                expr_heap.walk(*test, f);
                expr_heap.walk(*then, f);
            }
            StmtNode::Expr(e) => expr_heap.walk(*e, f),
            StmtNode::While { test, then } => {
                expr_heap.walk(*test, f);
                expr_heap.walk(*then, f)
            }
        }
    }
}

impl Stmt {
    pub fn new(node: StmtNode, meta: ast::Meta) -> Self {
        Self { node, meta }
    }
}

pub type CaseArm<'s> = ast::CaseArm_<'s, ExprId>;

pub struct Function<'s> {
    pub var_cnt: usize,
    pub type_: ast::FunctionType<'s>,
    pub body: ExprId,
    pub meta: ast::Meta,
    pub captures: Vec<LocalId>,
    pub local_cnt: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct ExprId(usize);

impl From<usize> for ExprId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Clone)]
pub struct Expr<'s> {
    pub node: ExprNode<'s>,
    pub type_: ast::Type<'s>,
    pub meta: ast::Meta,
}

pub struct ExprHeap<'s>(Vec<Expr<'s>>);

impl<'s> std::ops::Index<ExprId> for ExprHeap<'s> {
    type Output = Expr<'s>;
    fn index(&self, index: ExprId) -> &Self::Output {
        &self.0[index.0]
    }
}

impl<'s> Expr<'s> {
    pub fn new_never(meta: &ast::Meta) -> Self {
        Expr {
            node: ExprNode::Never,
            type_: ast::Type::new_never(),
            meta: meta.clone(),
        }
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

    pub fn get(&self, index: ExprId) -> Option<&Expr<'s>> {
        self.0.get(index.0)
    }
    pub fn get_mut(&mut self, index: ExprId) -> Option<&mut Expr<'s>> {
        self.0.get_mut(index.0)
    }

    pub fn walk(&mut self, root: ExprId, f: &mut impl FnMut(&mut Expr<'s>)) {
        f(self.get_mut(root).unwrap());

        use ExprNode::*;
        let node = self.get(root).unwrap().node.clone();
        match node {
            Apply { callee, args } => {
                self.walk(callee, f);
                args.iter().for_each(|id| self.walk(*id, f));
            }
            IfElse { test, then, else_ } => {
                self.walk(test, f);
                self.walk(then, f);
                self.walk(else_, f);
            }
            Case { operand, arms } => {
                self.walk(operand, f);
                arms.iter().for_each(|arm| self.walk(arm.result, f));
            }
            LetIn { value, in_, .. } => {
                self.walk(value, f);
                self.walk(in_, f);
            }
            LetPatIn { value, in_, .. } => {
                self.walk(value, f);
                self.walk(in_, f);
            }
            Tuple(elems) => elems.iter().for_each(|id| self.walk(*id, f)),
            Seq(stmts, ret) => {
                stmts.iter().for_each(|stmt| stmt.walk(self, f));
                if let Some(r) = ret {
                    self.walk(r, f);
                }
            }
            _ => {}
        };
    }

    pub fn dbg_expr(&mut self, root: ExprId) {
        self.walk(root, &mut |expr| {
            let span = (expr.meta.span.0)..(expr.meta.span.0 + expr.meta.span.1);
            let report = miette::miette!(
                labels = vec![miette::LabeledSpan::at(
                    span,
                    format!("{}", expr.type_.node)
                )],
                "Expr here"
            )
            .with_source_code(expr.meta.named_source());
            eprintln!("{:?}", report);
        })
    }
}

pub struct FunctionTable<'s> {
    tab: HashMap<FunctionId<'s>, Function<'s>>,
    infering: Vec<FunctionId<'s>>,
}
impl<'s> FunctionTable<'s> {
    pub fn new() -> Self {
        Self {
            tab: HashMap::new(),
            infering: Vec::new(),
        }
    }
    pub fn currently_infering(&self) -> Option<&FunctionId> {
        self.infering.last()
    }
    pub fn insert(&mut self, id: FunctionId<'s>, f: Function<'s>) {
        self.tab.insert(id, f);
    }
    pub fn get(&self, id: &FunctionId<'s>) -> Option<&Function<'s>> {
        self.tab.get(id)
    }
    pub fn begin_infering(&mut self, name: FunctionId<'s>) {
        self.infering.push(name);
    }
    pub fn is_infering(&self, name: &FunctionId) -> bool {
        self.infering.iter().find(|x| *x == name).is_some()
    }
    pub fn finish_infering(&mut self) {
        self.infering.pop().unwrap();
    }
    pub fn iter(&self) -> impl Iterator<Item = (&FunctionId, &Function<'s>)> {
        self.tab.iter()
    }
    pub fn contains(&self, id: &FunctionId<'s>) -> bool {
        self.tab.contains_key(id)
    }
}

use ast::Trie;

pub struct Symbols<'s> {
    pub functions: FunctionTable<'s>,
    pub constructors: Trie<ConstructorName<'s>, ast::Constructor<'s>, &'s str>,
    pub datas: Trie<DataName<'s>, ast::Data<'s>, &'s str>,
    pub builtins: Trie<BuiltinFunctionName<'s>, ast::BuiltinFunction<'s>, &'s str>,
    pub relations: Trie<RelationName<'s>, ast::Relation<'s>, &'s str>,
    pub instances: ast::InstanceIdTable<'s, ast::Instance<'s>>,
    pub methods: Trie<FunctionName<'s>, RelationName<'s>, &'s str>,
}

#[derive(Debug, Clone)]
pub struct Tag<'s>(u8, u8, ConstructorName<'s>);

impl<'s> std::hash::Hash for Tag<'s> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u8(self.0)
    }
}

impl<'s> Tag<'s> {
    pub fn name(&self) -> String {
        self.2.to_string()
    }
    pub fn code(&self) -> u8 {
        self.0
    }
    pub fn possibilities(&self) -> u8 {
        self.1
    }
}

impl<'s> Symbols<'s> {
    pub fn function_names(&self) -> impl Iterator<Item = &FunctionId> {
        self.functions.tab.keys()
    }
    pub fn new(symbols: ast::Symbols<'s>, functions: FunctionTable<'s>) -> Self {
        Self {
            functions,
            constructors: symbols.constructors,
            datas: symbols.datas,
            builtins: symbols.builtins,
            relations: symbols.relations,
            instances: symbols.instances,
            methods: symbols.methods,
        }
    }
    pub fn tag_for(&self, constructor: ConstructorName<'s>) -> Tag<'s> {
        let data_name = &self.constructors.get(&constructor).unwrap().belongs_to;
        let data = self.datas.get(&data_name).unwrap();
        Tag(
            data.constructors
                .iter()
                .position(|c| *c == constructor)
                .unwrap() as u8,
            data.constructors.len() as u8,
            constructor,
        )
    }

    pub fn function(&self, id: &FunctionId<'s>) -> &Function<'s> {
        self.functions.get(id).unwrap()
    }
}
