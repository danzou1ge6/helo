use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

use crate::lir::{self, ssa, TempIdVec};
use lir::BlockId;

use bitvec::prelude as bv;

type BitVec = bv::BitVec<u32, bv::Msb0>;

#[derive(Debug)]
pub struct ImmediateDominators(BlockIdVec<Option<BlockId>>);

impl std::ops::Index<BlockId> for ImmediateDominators {
    type Output = BlockId;
    fn index(&self, index: BlockId) -> &Self::Output {
        self.0[index].as_ref().unwrap()
    }
}

impl ImmediateDominators {
    pub fn set(&mut self, idx: BlockId, value: BlockId) {
        self.0[idx] = Some(value)
    }
    fn get(&self, idx: BlockId) -> Option<BlockId> {
        self.0[idx]
    }
    pub fn intersect(
        &self,
        mut b1: BlockId,
        mut b2: BlockId,
        order: &BlocksOrder<PostOrder>,
    ) -> BlockId {
        while b1 != b2 {
            while order[b1] < order[b2] {
                b1 = self[b1];
            }
            while order[b2] < order[b1] {
                b2 = self[b2];
            }
        }
        b1
    }
    pub fn build(blocks: &lir::BlockHeap, entry: BlockId, order: &BlocksOrder<PostOrder>) -> Self {
        let dom = BlockIdVec::repeat(None, blocks.len());
        let mut dom = Self(dom);
        dom.set(entry, entry);

        let mut changed = true;
        while changed {
            changed = false;
            for b in order.iter().rev() {
                if b == entry {
                    continue;
                }

                let mut pred_processed = blocks[b].predessorss().filter(|p| dom.get(*p).is_some());

                // Postorder ensures that at least one predecessor comes after it in postorder sequence, so
                // here at least one predecessor has been processed and assigned a immediate dominator in `dom`
                let new_idom = pred_processed.next().unwrap();
                let new_idom =
                    pred_processed.fold(new_idom, |idom, p| dom.intersect(idom, p, order));

                if dom.get(b) != Some(new_idom) {
                    dom.set(b, new_idom);
                    changed = true;
                }
            }
        }

        dom
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn edges<'a>(&'a self) -> impl Iterator<Item = (BlockId, BlockId)> + 'a {
        self.0
            .iter()
            .enumerate()
            .map(|(i, idom)| (BlockId(i), idom.unwrap()))
    }
}

pub struct DominanceTree(BlockIdVec<Vec<BlockId>>);

impl DominanceTree {
    pub fn from_idoms(dom: &ImmediateDominators) -> Self {
        let mut nodes = BlockIdVec::repeat(Vec::new(), dom.len());
        for (to, from) in dom.edges() {
            if from != to {
                nodes[from].push(to);
            }
        }
        Self(nodes)
    }
}

impl std::ops::Index<BlockId> for DominanceTree {
    type Output = Vec<BlockId>;
    fn index(&self, index: BlockId) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Debug)]
pub struct DominanceFrontier(BlockIdVec<HashSet<BlockId>>);

impl std::ops::Index<BlockId> for DominanceFrontier {
    type Output = HashSet<BlockId>;
    fn index(&self, index: BlockId) -> &Self::Output {
        &self.0[index]
    }
}

impl DominanceFrontier {
    pub fn build(blocks: &lir::BlockHeap, dominance_tree: &ImmediateDominators) -> Self {
        let mut frontiers = BlockIdVec::repeat(HashSet::new(), blocks.len());

        for block_id in blocks.iter_id() {
            let predecessors = blocks[block_id].predessorss().collect::<Vec<_>>();
            if predecessors.len() < 2 {
                continue;
            }

            for pred in predecessors {
                let mut runner = pred;
                while runner != dominance_tree[block_id] {
                    frontiers[runner].insert(block_id);
                    runner = dominance_tree[runner];
                }
            }
        }

        Self(frontiers)
    }
}

#[derive(Debug)]
pub struct PostOrder {}

#[derive(Debug)]
pub struct BlocksOrder<F> {
    /// `orders[i]` is the index where block i show up in `seq`
    orders: BlockIdVec<usize>,
    /// The sequence of blocks in some order
    seq: Vec<BlockId>,
    _markder: PhantomData<F>,
}

impl<F> std::ops::Index<BlockId> for BlocksOrder<F> {
    type Output = usize;
    fn index(&self, index: BlockId) -> &Self::Output {
        &self.orders[index]
    }
}

impl<F> BlocksOrder<F> {
    pub fn iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = BlockId> + DoubleEndedIterator<Item = BlockId> + 'a {
        self.seq.iter().copied()
    }
}

impl BlocksOrder<PostOrder> {
    pub fn post_order_of(blocks: &lir::BlockHeap, entry: BlockId) -> Self {
        fn post_order_sequence(
            begin: BlockId,
            blocks: &lir::BlockHeap,
            seq: &mut Vec<BlockId>,
            visited: &mut BitVec,
        ) {
            visited.set(begin.0, true);
            for susc in blocks[begin].successors() {
                if !visited[susc.0] {
                    post_order_sequence(susc, blocks, seq, visited);
                }
            }
            seq.push(begin);
        }

        // This is the order each block appear
        let mut seq = Vec::new();
        let mut visited = BitVec::repeat(false, blocks.len());
        post_order_sequence(entry, &blocks, &mut seq, &mut visited);

        let mut orders = BlockIdVec::repeat(0, blocks.len());

        for (place, block_id) in seq.iter().copied().enumerate() {
            orders[block_id] = place;
        }

        Self {
            orders,
            seq,
            _markder: PhantomData,
        }
    }
}

use lir::{BlockHeap_, Instruction, Jump, TempId};

use super::BlockIdVec;

#[derive(Clone, Default, Debug)]
pub struct Phi(pub TempId, pub Vec<TempId>);

#[derive(Debug)]
pub struct SsaBlock {
    pub(crate) phis: Vec<Phi>,
    pub(crate) body: imbl::Vector<Instruction>,
    pub(crate) pred: Vec<BlockId>,
    pub(crate) exit: Jump,
    pub(crate) tail_copies: Vec<(TempId, TempId)>,
}

impl SsaBlock {
    fn defs(&self) -> HashSet<TempId> {
        self.body
            .iter()
            .map(|inst| inst.def())
            .chain(self.phis.iter().map(|Phi(def, _)| *def))
            .fold(HashSet::new(), |mut defs, def| {
                defs.insert(def);
                defs
            })
    }
    fn successors<'a>(&'a self) -> Box<dyn Iterator<Item = BlockId> + 'a> {
        self.exit.successors()
    }
    fn last_inst_idx(&self) -> usize {
        self.body.len() - 1
    }
    fn inst(&self, idx: usize) -> &Instruction {
        &self.body[idx]
    }
    fn new(jump: Jump) -> SsaBlock {
        Self {
            phis: Vec::new(),
            body: imbl::Vector::new(),
            pred: Vec::new(),
            exit: jump,
            tail_copies: Vec::new(),
        }
    }
}

impl From<lir::Block> for SsaBlock {
    fn from(value: lir::Block) -> Self {
        Self {
            phis: Vec::new(),
            body: value.body,
            pred: value.pred.into_iter().collect(),
            exit: value.exit.unwrap(),
            tail_copies: Vec::new(),
        }
    }
}

pub type SsaBlockHeap = BlockHeap_<SsaBlock>;

impl From<lir::BlockHeap> for SsaBlockHeap {
    fn from(value: lir::BlockHeap) -> Self {
        let blocks = value
            .into_iter()
            .map(|b| SsaBlock::from(b))
            .collect::<BlockIdVec<_>>();
        Self(blocks)
    }
}

fn insert_phis(
    blocks: &mut SsaBlockHeap,
    dom_frontier: &DominanceFrontier,
    temp_cnt: usize,
    block_defs: &BlockIdVec<HashSet<TempId>>,
) {
    let mut def_sites = TempIdVec::repeat(HashSet::new(), temp_cnt);

    for block in blocks.iter_id() {
        for def in block_defs[block].iter().copied() {
            def_sites[def].insert(block);
        }
    }

    let mut phi_sites = TempIdVec::repeat(HashSet::new(), temp_cnt);

    for temp in def_sites.iter_index() {
        let mut work_list = def_sites[temp].iter().copied().collect::<Vec<_>>();

        while let Some(block) = work_list.pop() {
            for y in dom_frontier[block].iter().copied() {
                if !phi_sites[temp].contains(&y) {
                    let y_pred_cnt = blocks[y].pred.len();
                    blocks[y].phis.push(Phi(temp, vec![temp; y_pred_cnt]));

                    phi_sites[temp].insert(y);
                    if !def_sites[temp].contains(&y) {
                        work_list.push(y);
                    }
                }
            }
        }
    }
}

pub fn construct_ssa(
    blocks: lir::BlockHeap,
    entry: BlockId,
    dom_frontier: &DominanceFrontier,
    dom_tree: &DominanceTree,
    mut temp_cnt: usize,
) -> (SsaBlockHeap, usize) {
    let mut ssa_blocks = SsaBlockHeap::from(blocks);
    let block_defs = ssa_blocks.iter().map(|b| b.defs()).collect();

    // Insert phi node of temp `v` to all domiance frontiers of blocks defining `v`
    insert_phis(&mut ssa_blocks, &dom_frontier, temp_cnt, &block_defs);

    let mut rename_stacks = TempIdVec::repeat(Vec::new(), temp_cnt);

    for i in 0..temp_cnt {
        rename_stacks[TempId(i)].push(TempId(i));
    }

    fn search(
        block: BlockId,
        ssa_blocks: &mut SsaBlockHeap,
        rename_stacks: &mut TempIdVec<Vec<TempId>>,
        temp_cnt: &mut usize,
        dom_tree: &DominanceTree,
        block_defs: &BlockIdVec<HashSet<TempId>>,
    ) {
        let mut new_temp = || TempId::new(temp_cnt);

        let mut push_new_rename = |original_temp: TempId, stack: &mut TempIdVec<Vec<TempId>>| {
            if stack[original_temp].len() == 0 {
                stack[original_temp].push(original_temp);
                original_temp
            } else {
                let new = new_temp();
                stack[original_temp].push(new);
                new
            }
        };

        for Phi(to, _) in ssa_blocks[block].phis.iter_mut() {
            *to = push_new_rename(*to, rename_stacks);
        }

        for inst in ssa_blocks[block].body.iter_mut() {
            inst.uses_mut()
                .for_each(|u| *u = rename_stacks[*u].last().copied().unwrap());
            *inst.def_mut() = push_new_rename(inst.def(), rename_stacks);
        }

        ssa_blocks[block].exit.use_mut().map(|u| {
            *u = rename_stacks[*u].last().copied().unwrap();
        });

        for susc_id in ssa_blocks[block]
            .successors()
            .collect::<Vec<_>>()
            .into_iter()
        {
            let j = ssa_blocks[susc_id]
                .pred
                .iter()
                .position(|p| *p == block)
                .unwrap();

            for Phi(_, from) in ssa_blocks[susc_id].phis.iter_mut() {
                let old = from[j];
                from[j] = rename_stacks[old].last().copied().unwrap();
            }
        }

        for child in dom_tree[block].iter().copied() {
            search(
                child,
                ssa_blocks,
                rename_stacks,
                temp_cnt,
                dom_tree,
                block_defs,
            );
        }

        for def in block_defs[block].iter().copied() {
            rename_stacks[def].pop().unwrap();
        }
    }

    search(
        entry,
        &mut ssa_blocks,
        &mut rename_stacks,
        &mut temp_cnt,
        &dom_tree,
        &block_defs,
    );

    edge_splitting(&mut ssa_blocks);

    (ssa_blocks, temp_cnt)
}

struct Graph {
    adj: TempIdVec<HashSet<TempId>>,
}

impl Graph {
    pub fn new(temp_cnt: usize) -> Self {
        let adj = TempIdVec::repeat(HashSet::new(), temp_cnt);

        Self { adj }
    }
    pub fn add_edge(&mut self, a: TempId, b: TempId) {
        self.adj[a].insert(b);
        self.adj[b].insert(a);
    }
    pub fn vertices_cnt(&self) -> usize {
        self.adj.len()
    }
    pub fn color_graph(&self, arity: usize) -> (TempIdVec<TempId>, usize) {
        let mut coloring_order = (0..self.vertices_cnt())
            .filter(|i| *i >= arity) // Parameter temps should not be colored
            .map(|i| i.into())
            .collect::<Vec<_>>();
        coloring_order.sort_unstable_by(|a, b| self.adj[*b].len().cmp(&self.adj[*a].len()));

        let mut colors = TempIdVec::repeat(0, self.vertices_cnt());
        let mut used_color_cnt = arity;
        let mut colored = TempIdVec::repeat(false, self.vertices_cnt());

        // Color parameter temps as parameter registers
        for i in 0..arity {
            colors[TempId(i)] = i;
            colored[TempId(i)] = true;
        }

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
            colored[i] = true;
        }
        (
            colors.into_iter().map(|i| i.into()).collect(),
            used_color_cnt,
        )
    }
}

fn edge_splitting(blocks: &mut SsaBlockHeap) {
    for block in blocks.iter_id() {
        // No critical edge leads to `block`
        if blocks[block].pred.len() <= 1 {
            continue;
        }

        for pred in blocks[block].pred.clone().into_iter() {
            // `pred` -> `block` is a critical edge
            if blocks[pred].successors().count() > 1 {
                let inserted = SsaBlock::new(Jump::Jump(block));
                let inserted = blocks.push(inserted);

                blocks[pred].exit.successors_mut().for_each(|b| {
                    if *b == block {
                        *b = inserted;
                    }
                });
                blocks[block].pred.iter_mut().for_each(|b| {
                    if *b == pred {
                        *b = inserted;
                    }
                });
            }
        }
    }
}

// WARNING There must be no critical edge
fn remove_phis(blocks: &mut SsaBlockHeap) {
    for block_id in blocks.iter_id() {
        let phis = std::mem::take(&mut blocks[block_id].phis);

        for Phi(ret, args) in phis.into_iter() {
            for (arg, pred) in args
                .into_iter()
                .zip(blocks[block_id].pred.clone().into_iter())
            {
                if arg != ret {
                    blocks[pred].tail_copies.push((arg, ret));
                }
            }
        }
    }
}

fn sequence_copies(blocks: &mut SsaBlockHeap, swap_temps: BlockIdVec<TempId>) {
    fn sequence(copies: &[(TempId, TempId)], swap_temp: TempId) -> Vec<(TempId, TempId)> {
        let mut source = HashMap::new();
        let mut location = HashMap::new();
        let mut to_do = HashSet::new();
        let mut ready = Vec::new();
        let mut seq = Vec::new();

        for (from, to) in copies.iter().copied() {
            location.insert(from, from);
            source.insert(to, from);
            to_do.insert(to);
        }

        for (_, to) in copies.iter().copied() {
            if !location.contains_key(&to) {
                ready.push(to);
            }
        }

        while !to_do.is_empty() {
            while let Some(to) = ready.pop() {
                to_do.remove(&to);

                let from = source.get(&to).copied().unwrap();
                let loc = location.get(&from).copied().unwrap();

                seq.push((loc, to));
                location.insert(from, to);
                if loc == from && source.get(&from).is_some() {
                    ready.push(from);
                }
            }

            if to_do.is_empty() {
                break;
            }

            let to = to_do.iter().next().copied().unwrap();
            seq.push((to, swap_temp));

            location.insert(to, swap_temp);
            ready.push(to);
        }

        seq
    }

    for block in blocks.iter_id() {
        if !blocks[block].phis.is_empty() {
            panic!("phi's should have been deconstructed");
        }

        let seq = sequence(&blocks[block].tail_copies, swap_temps[block]);
        for (from, to) in seq.into_iter() {
            blocks[block].body.push_back(Instruction::Mov(to, from))
        }
    }
}

fn liveness_analysis(blocks: &SsaBlockHeap, temp_cnt: usize) -> (Graph, BlockIdVec<TempId>) {
    let mut graph = Graph::new(temp_cnt);
    let mut block_visited = vec![vec![false; blocks.len()]; temp_cnt];
    let mut swap_temps = BlockIdVec::repeat(TempId::default(), blocks.len());

    fn visit(block: BlockId, t: TempId, visited: &mut Vec<Vec<bool>>) {
        visited[t.0][block.0] = true;
    }
    fn get_visit(block: BlockId, t: TempId, visited: &Vec<Vec<bool>>) -> bool {
        visited[t.0][block.0]
    }

    fn live_out_at_inst(
        block: BlockId,
        inst_idx: usize,
        blocks: &SsaBlockHeap,
        t: TempId,
        graph: &mut Graph,
        swap_temps: &mut BlockIdVec<TempId>,
        visited: &mut Vec<Vec<bool>>,
    ) {
        let def = blocks[block].inst(inst_idx).def();
        if def != t {
            graph.add_edge(t, def);
            live_in_at_inst(block, inst_idx, blocks, t, graph, swap_temps, visited);
        }
    }

    fn live_in_at_inst(
        block: BlockId,
        inst_idx: usize,
        blocks: &SsaBlockHeap,
        t: TempId,
        graph: &mut Graph,
        swap_temps: &mut BlockIdVec<TempId>,
        visited: &mut Vec<Vec<bool>>,
    ) {
        if inst_idx == 0 {
            live_out_at_phis(block, blocks, t, graph, swap_temps, visited);
        } else {
            live_out_at_inst(block, inst_idx - 1, blocks, t, graph, swap_temps, visited);
        }
    }

    fn live_out_at_phis(
        block: BlockId,
        blocks: &SsaBlockHeap,
        t: TempId,
        graph: &mut Graph,
        swap_temps: &mut BlockIdVec<TempId>,
        visited: &mut Vec<Vec<bool>>,
    ) {
        if blocks[block]
            .phis
            .iter()
            .map(|Phi(ret, _)| {
                if *ret != t {
                    graph.add_edge(t, *ret);
                }
                *ret == t
            })
            .any(|x| x)
        {
            return;
        } else {
            for pred_block in blocks[block].pred.iter() {
                live_out_at_block(*pred_block, blocks, t, graph, swap_temps, visited);
            }
        }
    }

    fn live_out_at_block(
        block: BlockId,
        blocks: &SsaBlockHeap,
        t: TempId,
        graph: &mut Graph,
        swap_temps: &mut BlockIdVec<TempId>,
        visited: &mut Vec<Vec<bool>>,
    ) {
        if !get_visit(block, t, &visited) {
            visit(block, t, visited);

            if !blocks[block].tail_copies.is_empty() {
                panic!("phi's can't be deconstructed before liveness analysis")
            }

            graph.add_edge(swap_temps[block], t);

            live_in_at_exit(block, blocks, t, graph, swap_temps, visited);
        }
    }

    fn live_in_at_exit(
        block: BlockId,
        blocks: &SsaBlockHeap,
        t: TempId,
        graph: &mut Graph,
        swap_temps: &mut BlockIdVec<TempId>,
        visited: &mut Vec<Vec<bool>>,
    ) {
        if blocks[block].body.len() == 0 {
            live_out_at_phis(block, blocks, t, graph, swap_temps, visited);
        } else {
            let last_inst_idx = blocks[block].last_inst_idx();
            live_out_at_inst(block, last_inst_idx, blocks, t, graph, swap_temps, visited)
        }
    }

    for block in blocks.iter_id() {
        for inst_idx in 0..blocks[block].body.len() {
            for use_ in blocks[block].inst(inst_idx).uses() {
                live_in_at_inst(
                    block,
                    inst_idx,
                    blocks,
                    use_,
                    &mut graph,
                    &mut swap_temps,
                    &mut block_visited,
                );
            }
        }

        for phi_idx in 0..blocks[block].phis.len() {
            for (i, use_) in blocks[block].phis[phi_idx].1.iter().copied().enumerate() {
                live_out_at_block(
                    blocks[block].pred[i],
                    blocks,
                    use_,
                    &mut graph,
                    &mut swap_temps,
                    &mut block_visited,
                );
            }
        }

        if blocks[block].tail_copies.len() != 0 {
            panic!("no tail copies should have been emitted before liveness analysis");
        }

        if let Some(use_) = blocks[block].exit.uses() {
            live_in_at_exit(
                block,
                blocks,
                use_,
                &mut graph,
                &mut swap_temps,
                &mut block_visited,
            );
        }
    }

    (graph, swap_temps)
}

/// Deconstruct SSA form, converting back to LIR.
/// Returns blocks for LIR and number of registers used
pub fn deconstruct_ssa(
    mut ssa_blocks: SsaBlockHeap,
    temp_cnt: usize,
    arity: usize,
) -> (lir::BlockHeap, usize) {
    let (graph, swap_temps) = liveness_analysis(&ssa_blocks, temp_cnt);
    let (color_scheme, reg_cnt) = graph.color_graph(arity);

    // Color temps
    for block in ssa_blocks.iter_id() {
        ssa_blocks[block]
            .phis
            .iter_mut()
            .for_each(|Phi(ret, args)| {
                *ret = color_scheme[*ret];
                args.iter_mut().for_each(|arg| *arg = color_scheme[*arg]);
            });
        ssa_blocks[block]
            .body
            .iter_mut()
            .for_each(|inst| inst.substitute_temps_with(&|t| color_scheme[t]));
        ssa_blocks[block]
            .exit
            .use_mut()
            .map(|u| *u = color_scheme[*u]);
    }
    let swap_temps = swap_temps.into_iter().map(|t| color_scheme[t]).collect();

    remove_phis(&mut ssa_blocks);
    sequence_copies(&mut ssa_blocks, swap_temps);

    let blocks = ssa_blocks
        .into_iter()
        .map(|sb| lir::Block {
            body: sb.body,
            pred: sb.pred.into_iter().collect(),
            exit: Some(sb.exit),
        })
        .collect();
    let blocks = lir::BlockHeap_(blocks);

    (blocks, reg_cnt)
}

pub struct Function {
    pub body: BlockId,
    pub blocks: SsaBlockHeap,
    pub arity: usize,
    pub meta: helo_parse::ast::Meta,
    pub name: crate::ir::StrId,
    pub temp_cnt: usize,
}
