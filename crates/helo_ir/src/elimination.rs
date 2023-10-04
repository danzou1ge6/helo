use crate::lir;

use std::collections::HashMap;

#[derive(Clone)]
struct UnionFind {
    store: imbl::Vector<usize>,
}

impl UnionFind {
    fn new(cnt: usize) -> Self {
        Self {
            store: (0..cnt).collect(),
        }
    }
    fn is_root(&self, id: usize) -> bool {
        self.store[id] == id
    }
    fn find(&self, id: lir::TempId) -> lir::TempId {
        let parent = self.store[id.0];
        if self.is_root(parent) {
            parent.into()
        } else {
            let root = self.find(parent.into());
            root
        }
    }
    /// Make `from` points to `to`
    fn union(&mut self, to: lir::TempId, from: lir::TempId) {
        let a_root = self.find(to);
        self.store[from.0] = a_root.0;
    }
    fn normalized(&self, mut inst: lir::Instruction) -> lir::Instruction {
        inst.execute_substite_args(self);
        inst.execute_substite_output(&|_| lir::TempId::from(0));
        inst
    }
}

impl lir::TempSubstitution for UnionFind {
    fn subs(&self, id: lir::TempId) -> lir::TempId {
        self.find(id)
    }
}

struct BindStack {
    binds: Vec<HashMap<lir::Instruction, lir::TempId>>,
    eqs: Vec<UnionFind>,
}

impl BindStack {
    fn new(temp_cnt: usize) -> Self {
        Self {
            binds: vec![HashMap::new()],
            eqs: vec![UnionFind::new(temp_cnt)],
        }
    }
    fn register(&mut self, inst: lir::Instruction, temp: lir::TempId) {
        self.binds.last_mut().unwrap().insert(inst, temp);
    }
    fn scoped<R>(&mut self, mut f: impl FnMut(&mut Self) -> R) -> R {
        self.binds.push(HashMap::new());
        self.eqs.push(self.eqs.last().unwrap().clone());

        let r = f(self);

        self.binds.pop().unwrap();
        self.eqs.pop().unwrap();
        r
    }
    fn lookup(&self, inst: &lir::Instruction) -> Option<lir::TempId> {
        for scope in self.binds.iter().rev() {
            if let Some(r) = scope.get(&inst) {
                return Some(*r);
            }
        }
        None
    }
    fn eqs(&self) -> &UnionFind {
        self.eqs.last().unwrap()
    }
    fn eqs_mut(&mut self) -> &mut UnionFind {
        self.eqs.last_mut().unwrap()
    }
}

/// Eliminate common expressions in LIR
///
/// This function only works on trees, not DAGs
pub fn common_expression_elimination(
    entry: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    temp_cnt: usize,
) {
    let mut binds = BindStack::new(temp_cnt);
    cee(entry, 0, blocks, &mut binds);
}

fn cee(block: lir::BlockId, idx: usize, blocks: &mut lir::BlockHeap, binds: &mut BindStack) {
    let output = blocks[block][idx].def();
    let inst = binds.eqs().normalized(blocks[block][idx].clone());

    if inst.functional() {
        // A functional instruction must have output
        let output = output.unwrap();
        if let lir::Instruction::Mov(_, from) = inst {
            blocks[block].remove(idx);
            // Now all reference of `to` should be pointed to `from`
            binds.eqs_mut().union(from, output);

            return cee(block, idx, blocks, binds);
        } else {
            if let Some(temp) = binds.lookup(&inst) {
                blocks[block].remove(idx);
                binds.eqs_mut().union(temp, output);

                return cee(block, idx, blocks, binds);
            } else {
                binds.register(inst, output);
            }
        }
    }
    blocks[block][idx].execute_substite_args(binds.eqs());

    // let mut suscs = blocks.suscessive(block, idx).collect::<Vec<_>>();

    // if suscs.len() == 1 {
    //     let (susc_block, susc_idx) = suscs.pop().unwrap();
    //     return cee(susc_block, susc_idx, blocks, binds);
    // }

    // for (susc_block, susc_idx) in suscs.into_iter() {
    //     binds.scoped(|binds| {
    //         cee(susc_block, susc_idx, blocks, binds);
    //     })
    // }
    unimplemented!()
}
