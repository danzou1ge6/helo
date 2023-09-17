use std::collections::HashSet;

use crate::ir;
use bitvec::prelude as bv;

type BitVec = bv::BitVec<u32, bv::Msb0>;

#[derive(Clone)]
pub struct Liveness(BitVec);

pub struct LifetimeStore {
    v: Vec<Liveness>,
    expr_nodes_cnt: usize,
}

impl Liveness {
    pub fn unset(&mut self, id: ir::LocalId) {
        self.0.set(id.0, false)
    }
    pub fn set(&mut self, id: ir::LocalId) {
        self.0.set(id.0, true)
    }
    pub fn empty(cnt: usize) -> Self {
        Self(BitVec::repeat(false, cnt))
    }
    pub fn with_set(cnt: usize, id: ir::LocalId) -> Self {
        let mut r = Self::empty(cnt);
        r.set(id);
        r
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn live_locals(&self) -> Vec<ir::LocalId> {
        self.0.iter().enumerate().fold(vec![], |mut accu, (i, b)| {
            if bool::from(*b) {
                accu.push(i.into());
            }
            accu
        })
    }
}

impl std::ops::BitOrAssign<&Liveness> for Liveness {
    fn bitor_assign(&mut self, rhs: &Liveness) {
        self.0 |= &rhs.0;
    }
}

impl From<BitVec> for Liveness {
    fn from(value: BitVec) -> Self {
        Self(value)
    }
}

impl std::ops::Index<ir::ExprId> for LifetimeStore {
    type Output = Liveness;
    fn index(&self, index: ir::ExprId) -> &Self::Output {
        &self.v[index.0]
    }
}

impl std::ops::IndexMut<ir::ExprId> for LifetimeStore {
    fn index_mut(&mut self, index: ir::ExprId) -> &mut Self::Output {
        &mut self.v[index.0]
    }
}

impl LifetimeStore {
    pub fn new(expr_nodes_cnt: usize) -> Self {
        let mut v = Vec::new();
        v.resize(expr_nodes_cnt, BitVec::EMPTY.into());
        Self { v, expr_nodes_cnt }
    }

    pub fn analyze(&mut self, expr: ir::ExprId, ir_nodes: &ir::ExprHeap) -> &Liveness {
        use ir::ExprNode::*;
        match ir_nodes[expr].node() {
            LetBind { local, value, in_ } => {
                let mut r = self.analyze(*value, ir_nodes).clone();
                r |= self.analyze(*in_, ir_nodes);
                r.unset(*local);
                self[expr] = r;
            }
            SwitchTag(operand, v, default) => {
                let mut r = self.analyze(*operand, ir_nodes).clone();
                v.iter().for_each(|(_, e)| {
                    r |= &self.analyze(*e, ir_nodes);
                });
                r |= &self.analyze(*default, ir_nodes);
                self[expr] = r;
            }
            Switch(operand, v, default) => {
                let mut r = self.analyze(*operand, ir_nodes).clone();
                v.iter().for_each(|(_, e)| {
                    r |= &self.analyze(*e, ir_nodes);
                });
                r |= &self.analyze(*default, ir_nodes);
                self[expr] = r;
            }
            Cond(v, default) => {
                let mut r = Liveness::empty(self.expr_nodes_cnt);
                v.iter().for_each(|(c, e)| {
                    r |= &self.analyze(*c, ir_nodes);
                    r |= &self.analyze(*e, ir_nodes);
                });
                r |= &self[*default];
                self[expr] = r;
            }
            IfElse { test, then, else_ } => {
                let mut r = self.analyze(*test, ir_nodes).clone();
                r |= &self.analyze(*then, ir_nodes);
                r |= &self.analyze(*else_, ir_nodes);
                self[expr] = r;
            }
            Call { callee, args } => {
                let mut r = self.analyze(*callee, ir_nodes).clone();
                args.iter()
                    .for_each(|arg| r |= &self.analyze(*arg, ir_nodes));
                self[expr] = r;
            }
            Immediate(_) => {}
            MakeClosure(_, locals) => {
                let mut r = Liveness::empty(self.expr_nodes_cnt);
                locals.iter().for_each(|cap| r.set(*cap));
                self[expr] = r;
            }
            Local(local) => {
                self[expr] = Liveness::with_set(self.expr_nodes_cnt, *local);
            }
            UserFunction(_) => {}
            Builtin(_) => {}
            VariantField(local, _) | TupleField(local, _) => {
                self[expr] = Liveness::with_set(self.expr_nodes_cnt, *local);
            }
            MakeTuple(elems) | MakeTagged(_, elems) => {
                let mut r = Liveness::empty(self.expr_nodes_cnt);
                elems
                    .iter()
                    .for_each(|arg| r |= &self.analyze(*arg, ir_nodes));
                self[expr] = r;
            }
            ThisClosure(_) => {}
            Panic(_) => {}
        };
        &self[expr]
    }

    pub fn liveness_iter(&self) -> impl Iterator<Item = &Liveness> {
        self.v.iter()
    }
}

pub struct Graph {
    adj: Vec<HashSet<usize>>,
}

impl Graph {
    pub fn new(local_cnt: usize) -> Self {
        let mut adj = Vec::new();
        adj.resize(local_cnt, HashSet::new());

        Self { adj }
    }
    pub fn add_edge(&mut self, a: usize, b: usize) {
        self.adj[a].insert(b);
        self.adj[b].insert(a);
    }
    pub fn vertices_cnt(&self) -> usize {
        self.adj.len()
    }
    pub fn color_graph(&self) -> Vec<usize> {
        let mut coloring_order = (0..self.vertices_cnt()).collect::<Vec<_>>();
        coloring_order.sort_unstable_by(|a, b| self.adj[*b].len().cmp(&self.adj[*a].len()));

        let mut colors = Vec::new();
        colors.resize(self.vertices_cnt(), 0);
        let mut used_color_cnt = 0;
        let mut colored = BitVec::repeat(false, self.vertices_cnt());

        for i in coloring_order {
            let used_colors = self.adj[i].iter().fold(HashSet::new(), |mut accu, vertix| {
                if colored[*vertix] {
                    accu.insert(colors[*vertix]);
                }
                accu
            });
            let mut need_new_color = true;
            for c in 0..used_color_cnt {
                if !used_colors.contains(&c) {
                    colors[i] = c;
                    need_new_color = false;
                    break;
                }
            }
            if need_new_color {
                colors[i] = used_color_cnt;
                used_color_cnt += 1;
            }
            colored.set(i, true);
        }
        colors
    }
}

fn build_intefere_graph(life: &LifetimeStore, local_cnt: usize) -> Graph {
    let mut g = Graph::new(local_cnt);

    for liveness in life.liveness_iter() {
        let live_locals = liveness.live_locals();
        for i in 0..live_locals.len() {
            for j in i..live_locals.len() {
                g.add_edge(live_locals[i].0, live_locals[j].0)
            }
        }
    }

    g
}
