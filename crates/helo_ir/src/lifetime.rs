use std::collections::HashSet;

use crate::lir;
use crate::lir::ssa;
use bitvec::prelude as bv;

type BitVec = bv::BitVec<u32, bv::Msb0>;

#[derive(Clone)]
struct Liveness(BitVec);

struct LifetimeStore {
    ins: lir::BlockIdVec<Vec<Liveness>>,
    outs: lir::BlockIdVec<Vec<Liveness>>,
    temp_cnt: usize,
}

impl Liveness {
    pub fn unset(&mut self, id: lir::TempId) {
        self.0.set(id.0, false)
    }
    pub fn set(&mut self, id: lir::TempId) {
        self.0.set(id.0, true)
    }
    pub fn empty(cnt: usize) -> Self {
        Self(BitVec::repeat(false, cnt))
    }
    pub fn live_locals(&self) -> Vec<lir::TempId> {
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

impl LifetimeStore {
    pub fn new(blocks: &lir::BlockHeap, temp_cnt: usize) -> Self {
        let ins = blocks
            .iter()
            .map(|b| vec![Liveness::empty(temp_cnt); b.len()])
            .collect();
        let outs = blocks
            .iter()
            .map(|b| vec![Liveness::empty(temp_cnt); b.len()])
            .collect();
        Self {
            ins,
            outs,
            temp_cnt,
        }
    }

    pub fn analyze(
        &mut self,
        block_id: lir::BlockId,
        blocks: &lir::BlockHeap,
        order: ssa::BlocksOrder<ssa::PostOrder>,
    ) {
        let mut changed = true;
        while changed {
            for block in order.iter() {
                unimplemented!()
            }
        }
    }

    pub fn liveness_iter(&self) -> impl Iterator<Item = &Liveness> {
        self.ins.iter().map(|bv| bv.iter()).flatten()
    }
}

struct Graph {
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
    pub fn color_graph(&self) -> (Vec<usize>, usize) {
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
        (colors, used_color_cnt)
    }
}

fn build_intefere_graph(life: &LifetimeStore, temp_cnt: usize) -> Graph {
    let mut g = Graph::new(temp_cnt);

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

pub fn compress_temps(entry: lir::BlockId, blocks: &mut lir::BlockHeap, temp_cnt: usize) -> usize {
    // let mut lifetime_store = LifetimeStore::new(blocks, temp_cnt);
    // lifetime_store.analyze(entry, blocks);
    // let interfere_graph = build_intefere_graph(&lifetime_store, temp_cnt);
    // let (color_scheme, cnt) = interfere_graph.color_graph();
    // let subs = lir::TempSubstitutionMap::from(color_scheme);
    // blocks.execute_substitution(&subs);
    // cnt
    unimplemented!()
}
