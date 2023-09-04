use std::collections::HashMap;

use crate::ast;

pub type ExprNode<'s> = ast::ExprNode_<'s, ExprId, ast::FunctionId, CaseArm<'s>>;

pub type CaseArm<'s> = ast::CaseArm_<'s, ExprId>;

pub struct Function<'s> {
    pub var_cnt: usize,
    pub type_: ast::FunctionType<'s>,
    pub body: ExprId,
    pub meta: ast::Meta,
}

#[derive(Debug, Clone, Copy)]
pub struct ExprId(usize);

impl From<usize> for ExprId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

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

        use ast::ExprNode_::*;
        let node = self.get(root).unwrap().node.clone();
        match node {
            Call { callee, args } => {
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
    tab: HashMap<String, Function<'s>>,
    infering: Vec<String>,
}
impl<'s> FunctionTable<'s> {
    pub fn new() -> Self {
        Self {
            tab: HashMap::new(),
            infering: Vec::new(),
        }
    }
    pub fn currently_infering(&self) -> Option<&str> {
        self.infering.last().map(|x| x.as_str())
    }
    pub fn insert(&mut self, name: String, f: Function<'s>) {
        self.tab.insert(name, f);
    }
    pub fn get(&self, name: &str) -> Option<&Function<'s>> {
        self.tab.get(name)
    }
    pub fn begin_infering(&mut self, name: String) {
        self.infering.push(name);
    }
    pub fn is_infering(&self, name: &str) -> bool {
        self.infering.iter().find(|x| x.as_str() == name).is_some()
    }
    pub fn finish_infering(&mut self) {
        self.infering.pop().unwrap();
    }
    pub fn iter(&self) -> impl Iterator<Item = (&String, &Function<'s>)> {
        self.tab.iter()
    }
}
