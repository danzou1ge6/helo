use crate::lir::BlockId;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Site {
    Phi(BlockId, usize),
    Inst(BlockId, usize),
    Exit(BlockId),
}

mod dead_code_elimination {
    use std::collections::HashSet;

    use crate::lir::{self, ssa, TempIdVec};

    use super::Site;

    use lir::TempId;
    fn collect_uses_defs(
        blocks: &ssa::SsaBlockHeap,
        temp_cnt: usize,
    ) -> (TempIdVec<HashSet<Site>>, TempIdVec<Option<Site>>) {
        let mut uses = lir::TempIdVec::repeat(HashSet::new(), temp_cnt);
        let mut defs = lir::TempIdVec::repeat(None, temp_cnt);

        for block in blocks.iter_id() {
            for (i, ssa::Phi(ret, args)) in blocks[block].phis.iter().enumerate() {
                defs[*ret] = Some(Site::Phi(block, i));
                args.iter().copied().for_each(|arg| {
                    uses[arg].insert(Site::Phi(block, i));
                });
            }

            for inst_id in 0..blocks[block].body.len() {
                let inst = &blocks[block].body[inst_id];
                let def = inst.def();
                defs[def] = Some(Site::Inst(block, inst_id));
                inst.uses().for_each(|u| {
                    uses[u].insert(Site::Inst(block, inst_id));
                })
            }

            if let Some(exit_use) = blocks[block].exit.uses() {
                uses[exit_use].insert(Site::Exit(block));
            }
        }

        (uses, defs)
    }

    pub fn run(blocks: &mut ssa::SsaBlockHeap, temp_cnt: usize) {
        let (mut uses, mut defs) = collect_uses_defs(blocks, temp_cnt);

        let mut work_list: Vec<_> = (0..temp_cnt).map(|i| TempId(i)).collect();
        while let Some(t) = work_list.pop() {
            if uses[t].len() == 0 {
                let def_site = defs[t];

                match def_site {
                    Some(Site::Phi(block, i)) => {
                        let ssa::Phi(ret, args) = blocks[block].phis.remove(i);
                        defs[ret] = None;
                        args.into_iter().for_each(|arg| {
                            uses[arg].remove(&Site::Phi(block, i));
                            work_list.push(arg);
                        });
                    }
                    Some(Site::Inst(block, i)) => {
                        let inst = blocks[block].body.remove(i);
                        defs[inst.def()] = None;
                        inst.uses().for_each(|u| {
                            uses[u].remove(&Site::Inst(block, i));
                            work_list.push(u);
                        });
                    }
                    Some(Site::Exit(..)) => unreachable!(),
                    None => {}
                }
            }
        }
    }
}

pub use dead_code_elimination::run as dead_code_elimination;

mod common_expression_elimination {

    use std::collections::HashMap;

    use crate::lir;
    use lir::ssa;
    use lir::{BlockId, Instruction, TempId};
    use ssa::{Phi, SsaBlockHeap};

    #[derive(Clone)]
    struct TempIdUnionFind(imbl::Vector<TempId>);

    impl TempIdUnionFind {
        fn new(temp_cnt: usize) -> Self {
            Self((0..temp_cnt).map(|i| i.into()).collect())
        }

        fn is_root(&self, t: TempId) -> bool {
            self.0[t.0] == t
        }

        fn find(&mut self, t: TempId) -> TempId {
            let parent = self.0[t.0];
            if self.is_root(parent) {
                return parent;
            }
            let root = self.find(parent);
            self.0[t.0] = root;
            root
        }

        fn find_immutable(&self, t: TempId) -> TempId {
            let parent = self.0[t.0];
            if self.is_root(parent) {
                return parent;
            }
            self.find_immutable(parent)
        }

        fn union(&mut self, t: TempId, point_to: TempId) {
            let root = self.find(t);
            self.0[root.0] = self.find(point_to);
        }
    }

    struct WalkContext {
        bindings: Vec<HashMap<Instruction, TempId>>,
        copies: Vec<TempIdUnionFind>,
    }

    impl WalkContext {
        fn new(temp_cnt: usize) -> Self {
            Self {
                bindings: vec![HashMap::new()],
                copies: vec![TempIdUnionFind::new(temp_cnt)],
            }
        }
        fn bind(&mut self, inst: Instruction, t: TempId) {
            self.bindings.last_mut().unwrap().insert(inst, t);
        }
        fn copy(&mut self, from: TempId, to: TempId) {
            self.copies.last_mut().unwrap().union(to, from);
        }
        fn lookup_binding(&self, inst: &Instruction) -> Option<TempId> {
            for b in self.bindings.iter().rev() {
                if let Some(t) = b.get(inst) {
                    return Some(*t);
                }
            }
            None
        }
        fn lookup_copy_root(&self, t: TempId) -> TempId {
            self.copies.last().unwrap().find_immutable(t)
        }
        fn scoped<R>(&mut self, mut f: impl FnMut(&mut WalkContext) -> R) -> R {
            self.bindings.push(HashMap::new());
            self.copies.push(self.copies.last().unwrap().clone());

            let r = f(self);

            self.bindings.pop().unwrap();
            self.copies.pop().unwrap();
            r
        }
        fn normalize(&self, inst: &Instruction) -> Instruction {
            let mut inst = inst.clone();
            *inst.def_mut() = TempId::default();
            inst
        }
    }

    pub fn run(
        blocks: &mut SsaBlockHeap,
        dom_tree: &ssa::DominanceTree,
        entry: BlockId,
        temp_cnt: usize,
    ) {
        fn walk_tree(
            block: BlockId,
            blocks: &mut SsaBlockHeap,
            dom_tree: &ssa::DominanceTree,
            ctx: &mut WalkContext,
        ) {
            let mut inst_idx = 0;
            while inst_idx < blocks[block].body.len() {
                let inst = &mut blocks[block].body[inst_idx];
                let inst_def = inst.def();

                if !inst.functional() {
                    inst_idx += 1;
                    continue;
                }

                inst.uses_mut().for_each(|u| *u = ctx.lookup_copy_root(*u));
                let norm_inst = ctx.normalize(inst);

                if let Some(existent_temp) = ctx.lookup_binding(&norm_inst) {
                    ctx.copy(existent_temp, inst.def());
                    blocks[block].body.remove(inst_idx);
                    continue;
                }

                if let Instruction::Mov(to, from) = &*inst {
                    ctx.copy(*from, *to);
                    blocks[block].body.remove(inst_idx);
                    continue;
                }

                ctx.bind(norm_inst, inst_def);

                inst_idx += 1;
            }

            blocks[block]
                .exit
                .use_mut()
                .map(|u| *u = ctx.lookup_copy_root(*u));

            for succ in blocks[block]
                .exit
                .successors()
                .collect::<Vec<_>>()
                .into_iter()
            {
                let mut phi_idx = 0;
                while phi_idx < blocks[succ].phis.len() {
                    let Phi(ret, args) = &mut blocks[succ].phis[phi_idx];
                    *ret = ctx.lookup_copy_root(*ret);
                    args.iter_mut()
                        .for_each(|arg| *arg = ctx.lookup_copy_root(*arg));
                    let args_first = *args.first().unwrap();

                    if args
                        .iter()
                        .fold(true, |accu, arg| accu & (*arg == args_first))
                    {
                        ctx.copy(args_first, *ret);
                        blocks[succ].phis.remove(phi_idx);
                        continue;
                    }

                    phi_idx += 1;
                }
            }

            for child in dom_tree[block].iter().copied() {
                ctx.scoped(|ctx| walk_tree(child, blocks, dom_tree, ctx));
            }
        }

        walk_tree(entry, blocks, dom_tree, &mut WalkContext::new(temp_cnt))
    }
}

pub use common_expression_elimination::run as common_expression_elimination;

mod constant_propgation {
    use crate::lir::{ssa::SsaBlockHeap, BlockId};

    use super::Site;

    enum Constant {
        Top,
        Bottom,
        CInt(i64),
        CBool(bool),
    }
}
