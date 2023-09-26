use std::collections::HashSet;

use crate::lir;
use bitvec::prelude as bv;

type BitVec = bv::BitVec<u32, bv::Msb0>;

#[derive(Clone)]
struct Liveness(BitVec);

struct LifetimeStore {
    v: Vec<Vec<Liveness>>,
    analyzed: Vec<BitVec>,
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

impl std::ops::Index<(lir::BlockId, usize)> for LifetimeStore {
    type Output = Liveness;
    fn index(&self, (block_id, idx): (lir::BlockId, usize)) -> &Self::Output {
        &self.v[block_id.0][idx]
    }
}

impl std::ops::IndexMut<(lir::BlockId, usize)> for LifetimeStore {
    fn index_mut(&mut self, (block_id, idx): (lir::BlockId, usize)) -> &mut Self::Output {
        &mut self.v[block_id.0][idx]
    }
}

impl LifetimeStore {
    pub fn new(blocks: &lir::BlockHeap, temp_cnt: usize) -> Self {
        let v = blocks
            .iter()
            .map(|b| vec![Liveness::empty(temp_cnt); b.len()])
            .collect();
        let analyzed = blocks
            .iter()
            .map(|b| BitVec::repeat(false, b.len()))
            .collect();
        Self {
            v,
            analyzed,
            temp_cnt,
        }
    }

    fn analyzed_flags_for_block(&mut self, block_id: lir::BlockId) -> &mut BitVec {
        &mut self.analyzed[block_id.0]
    }

    /// Assuming that the LIR control flow graph forms a DAG
    fn analyze_instruction(&mut self, block_id: lir::BlockId, idx: usize, blocks: &lir::BlockHeap) {
        for (susc_bid, susc_idx) in blocks.suscessive(block_id, idx) {
            if !self.analyzed_flags_for_block(susc_bid)[susc_idx] {
                self.analyze_instruction(susc_bid, susc_idx, blocks);
            }

            let mut susc_liveness = Liveness::empty(self.temp_cnt);
            std::mem::swap(&mut self[(susc_bid, susc_idx)], &mut susc_liveness);
            self[(block_id, idx)] |= &susc_liveness;
            std::mem::swap(&mut self[(susc_bid, susc_idx)], &mut susc_liveness);

            if let Some(out) = blocks[susc_bid][susc_idx].output() {
                self[(block_id, idx)].unset(out);
            }
        }

        for input in blocks[block_id][idx].input() {
            self[(block_id, idx)].set(input);
        }

        self.analyzed_flags_for_block(block_id).set(idx, true);
    }

    pub fn analyze(&mut self, block_id: lir::BlockId, blocks: &lir::BlockHeap) {
        self.analyze_instruction(block_id, 0, blocks)
    }

    pub fn liveness_iter(&self) -> impl Iterator<Item = &Liveness> {
        self.v.iter().map(|bv| bv.iter()).flatten()
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

pub fn compress_temps(entry: lir::BlockId, blocks: &mut lir::BlockHeap, temp_cnt: usize) {
    let mut lifetime_store = LifetimeStore::new(blocks, temp_cnt);
    lifetime_store.analyze(entry, blocks);
    let interfere_graph = build_intefere_graph(&lifetime_store, temp_cnt);
    let color_scheme = interfere_graph.color_graph();
    blocks.execute_substitution(&color_scheme.into());
}
