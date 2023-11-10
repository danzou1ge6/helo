use std::collections::HashSet;

use crate::lir::BlockId;
use crate::lir::{self, ssa, TempIdVec};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Site {
    Phi(BlockId, usize),
    Inst(BlockId, usize),
    Exit(BlockId),
}

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

mod dead_code_elimination {

    use super::collect_uses_defs;
    use crate::lir;
    use crate::lir::ssa;
    use crate::lir::TempId;

    use super::Site;
    pub fn run(blocks: &mut ssa::SsaBlockHeap, temp_cnt: usize) {
        let (_,  defs) = collect_uses_defs(blocks, temp_cnt);

        let mut phi_markers: lir::BlockIdVec<Vec<bool>> = blocks
            .iter_id()
            .map(|id| vec![false; blocks[id].phis.len()])
            .collect();
        let mut inst_markers: lir::BlockIdVec<Vec<bool>> = blocks
            .iter_id()
            .map(|id| vec![false; blocks[id].body.len()])
            .collect();

        fn mark_site(
            site: Site,
            defs: &lir::AltIdxVec<Option<Site>, TempId>,
            inst_markers: &mut lir::BlockIdVec<Vec<bool>>,
            phi_markers: &mut lir::BlockIdVec<Vec<bool>>,
            blocks: &ssa::SsaBlockHeap,
        ) {
            match site {
                Site::Inst(block_id, idx) => {
                    mark_inst(block_id, idx, defs, inst_markers, phi_markers, blocks)
                }
                Site::Phi(block_id, idx) => {
                    mark_phi(block_id, idx, defs, inst_markers, phi_markers, blocks)
                }
                Site::Exit(..) => unreachable!(),
            }
        }

        fn mark_inst(
            block_id: lir::BlockId,
            idx: usize,
            defs: &lir::AltIdxVec<Option<Site>, TempId>,
            inst_markers: &mut lir::BlockIdVec<Vec<bool>>,
            phi_markers: &mut lir::BlockIdVec<Vec<bool>>,
            blocks: &ssa::SsaBlockHeap,
        ) {
            if !inst_markers[block_id][idx] {
                inst_markers[block_id][idx] = true;
                for u in blocks[block_id].body[idx].uses() {
                    if let Some(def) = defs[u] {
                        mark_site(def, defs, inst_markers, phi_markers, blocks)
                    }
                }
            }
        }

        fn mark_phi(
            block_id: lir::BlockId,
            idx: usize,
            defs: &lir::AltIdxVec<Option<Site>, TempId>,
            inst_markers: &mut lir::BlockIdVec<Vec<bool>>,
            phi_markers: &mut lir::BlockIdVec<Vec<bool>>,
            blocks: &ssa::SsaBlockHeap,
        ) {
            if !phi_markers[block_id][idx] {
                phi_markers[block_id][idx] = true;
                let ssa::Phi(_, args) = &blocks[block_id].phis[idx];

                for u in args {
                    if let Some(def) = defs[*u] {
                        mark_site(def, defs, inst_markers, phi_markers, blocks)
                    }
                }
            }
        }

        for block_id in blocks.iter_id() {
            for (inst_idx, inst) in blocks[block_id].body.iter().enumerate() {
                if !inst.functional() {
                    mark_inst(block_id, inst_idx, &defs, &mut inst_markers, &mut phi_markers, blocks)
                }
            }
            if let Some(u) = blocks[block_id].exit.uses() {
                if let Some(def) = defs[u] {
                    mark_site(def, &defs, &mut inst_markers, &mut phi_markers, &blocks);
                }
            }
        }

        for block_id in blocks.iter_id() {
            blocks[block_id].phis = std::mem::take(&mut blocks[block_id].phis)
                .into_iter()
                .enumerate()
                .filter(|(i, _)| phi_markers[block_id][*i])
                .map(|(_, p)| p)
                .collect();

            blocks[block_id].body = std::mem::take(&mut blocks[block_id].body)
                .into_iter()
                .enumerate()
                .filter(|(i, _)| inst_markers[block_id][*i])
                .map(|(_, inst)| inst)
                .collect();
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

                inst.uses_mut().for_each(|u| *u = ctx.lookup_copy_root(*u));

                if !inst.functional() {
                    inst_idx += 1;
                    continue;
                }

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

mod constant_propagation {
    use std::collections::HashSet;

    use crate::lir::BlockTopology;
    use crate::lir::{self, ssa, BlockId};
    use lir::Instruction;
    use lir::TempId;

    use super::Site;

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Value {
        Top,
        Bottom,
        CInt(i64),
        CBool(bool),
        CTag(u8),
        CChar(char),
    }

    impl Value {
        fn inst(&self, to: TempId) -> Option<Instruction> {
            match self {
                Value::CInt(i) => Some(Instruction::Int(to, *i)),
                Value::CBool(b) => Some(Instruction::Bool(to, *b)),
                Value::CChar(c) => Some(Instruction::Char(to, *c)),
                _ => None,
            }
        }
        fn lower_then(&self, rhs: &Value) -> bool {
            use Value::*;
            match (self, rhs) {
                (Bottom, rhs) if !matches!(rhs, Bottom) => true,
                (lhs, Top) if !matches!(lhs, Top) => true,
                _ => false,
            }
        }
        fn is_constant(&self) -> bool {
            use Value::*;
            match self {
                CInt(_) | CBool(_) | CTag(_) | CChar(_) => true,
                _ => false,
            }
        }
    }

    fn update_value(
        blocks: &ssa::SsaBlockHeap,
        temp: TempId,
        def_site: Site,
        values: &mut lir::TempIdVec<Value>,
    ) {
        if let Site::Inst(block_id, inst_idx) = def_site {
            match &blocks[block_id].body[inst_idx] {
                Instruction::Int(_, x) => values[temp] = Value::CInt(*x),
                Instruction::Bool(_, x) => values[temp] = Value::CBool(*x),
                Instruction::Char(_, c) => values[temp] = Value::CChar(*c),
                Instruction::Tagged(_, tag, _) => values[temp] = Value::CTag(*tag),
                _ => {}
            }
        }
    }

    fn process_phi(
        blocks: &ssa::SsaBlockHeap,
        block_id: BlockId,
        phi_idx: usize,
        values: &mut lir::TempIdVec<Value>,
        block_run: &lir::BlockIdVec<bool>,
        temps_worklist: &mut HashSet<TempId>,
    ) {
        let ssa::Phi(to, args) = &blocks[block_id].phis[phi_idx];

        // If some predecessor is executable and it sends a top to this phi, the result of the phi is Top
        if blocks[block_id]
            .pred
            .iter()
            .zip(args.iter())
            .any(|(pred_block_id, arg)| {
                matches!(values[*arg], Value::Top) && block_run[*pred_block_id]
            })
        {
            set_value(*to, Value::Top, values, temps_worklist);
            return;
        }

        // Those executable and are constants
        let mut constants =
            blocks[block_id]
                .pred
                .iter()
                .zip(args.iter())
                .filter(|(pred_block_id, arg)| {
                    block_run[**pred_block_id] && values[**arg].is_constant()
                });

        if let Some((_, first)) = constants.next() {
            if constants.all(|(_, temp)| values[*temp] == values[*first]) {
                set_value(*to, values[*first], values, temps_worklist);
            }
        }
    }

    fn set_value(
        temp: TempId,
        value: Value,
        values: &mut lir::AltIdxVec<Value, TempId>,
        temps_worklist: &mut HashSet<TempId>,
    ) {
        if values[temp].lower_then(&value) {
            temps_worklist.insert(temp);
        }
        values[temp] = value;
    }

    fn process_inst(
        blocks: &ssa::SsaBlockHeap,
        block_id: BlockId,
        inst_idx: usize,
        values: &mut lir::TempIdVec<Value>,
        _block_run: &lir::BlockIdVec<bool>,
        temps_worklist: &mut HashSet<TempId>,
    ) {
        let default_case_for_inst =
            |inst: &Instruction,
             values: &mut lir::AltIdxVec<Value, TempId>,
             temps_worklist: &mut HashSet<TempId>| {
                if !inst.uses().all(|u| matches!(values[u], Value::Bottom)) {
                    set_value(inst.def(), Value::Top, values, temps_worklist)
                }
            };

        match &blocks[block_id].body[inst_idx] {
            Instruction::Mov(to, from) => {
                set_value(*to, values[*from], values, temps_worklist);
            }
            inst @ Instruction::CallBuiltin(ret, builtin, args) if args.len() == 2 => {
                let (a, b) = (args[0], args[1]);

                use helo_runtime::builtins;
                use Value::*;

                let int_airth =
                    |op: fn(i64, i64) -> i64,
                     values: &mut lir::AltIdxVec<Value, TempId>,
                     temps_worklist: &mut HashSet<TempId>| {
                        if let (CInt(a), CInt(b)) = (values[a], values[b]) {
                            set_value(*ret, CInt(op(a, b)), values, temps_worklist);
                        }
                    };

                let int_relation =
                    |op: fn(&i64, &i64) -> bool,
                     values: &mut lir::AltIdxVec<Value, TempId>,
                     temps_worklist: &mut HashSet<TempId>| {
                        if let (CInt(a), CInt(b)) = (values[a], values[b]) {
                            set_value(*ret, CBool(op(&a, &b)), values, temps_worklist);
                        }
                    };

                let bool_arith =
                    |op: fn(bool, bool) -> bool,
                     values: &mut lir::AltIdxVec<Value, TempId>,
                     temps_worklist: &mut HashSet<TempId>| {
                        if let (CBool(a), CBool(b)) = (values[a], values[b]) {
                            set_value(*ret, CBool(op(a, b)), values, temps_worklist);
                        }
                    };

                use std::ops::{Add, BitAnd, BitOr, Div, Mul, Sub};
                match builtins::name(*builtin) {
                    "+" => int_airth(i64::add, values, temps_worklist),
                    "-" => int_airth(i64::sub, values, temps_worklist),
                    "*" => int_airth(i64::mul, values, temps_worklist),
                    "/" => int_airth(i64::div, values, temps_worklist),
                    "==" => int_relation(i64::eq, values, temps_worklist),
                    "/=" => int_relation(i64::ne, values, temps_worklist),
                    ">=" => int_relation(i64::ge, values, temps_worklist),
                    "<=" => int_relation(i64::le, values, temps_worklist),
                    ">" => int_relation(i64::gt, values, temps_worklist),
                    "<" => int_relation(i64::lt, values, temps_worklist),
                    "mod" => int_airth(i64::rem_euclid, values, temps_worklist),
                    "&&" => bool_arith(bool::bitand, values, temps_worklist),
                    "||" => bool_arith(bool::bitor, values, temps_worklist),
                    _ => default_case_for_inst(inst, values, temps_worklist),
                }
            }
            inst @ Instruction::CallBuiltin(ret, builtin, args) if args.len() == 0 => {
                let a = args[0];

                use helo_runtime::builtins;
                use Value::*;

                match builtins::name(*builtin) {
                    "not" => {
                        if let CBool(a) = values[a] {
                            set_value(*ret, CBool(!a), values, temps_worklist);
                        }
                    }
                    _ => default_case_for_inst(inst, values, temps_worklist),
                }
            }
            inst => default_case_for_inst(inst, values, temps_worklist),
        }
    }

    fn process_jump(
        blocks: &ssa::SsaBlockHeap,
        block_id: BlockId,
        values: &lir::TempIdVec<Value>,
        block_run: &mut lir::BlockIdVec<bool>,
        blocks_worklist: &mut HashSet<BlockId>,
    ) {
        let mut lift_to_run = |block_id, blocks: &ssa::SsaBlockHeap| {
            if block_run[block_id] == false {
                block_run[block_id] = true;
                blocks_worklist.insert(block_id);
            }
            for susc in blocks[block_id].successors().filter(|b| !block_run[*b]) {
                blocks_worklist.insert(susc);
            }
        };

        use lir::Jump;
        match &blocks[block_id].exit {
            Jump::JumpTable(temp, table) => {
                if let Value::CTag(tag) = values[*temp] {
                    let to_block_id = table[tag as usize];
                    lift_to_run(to_block_id, &blocks);
                } else {
                    table
                        .into_iter()
                        .for_each(|to_block_id| lift_to_run(*to_block_id, &blocks));
                }
            }
            Jump::JumpIfElse(temp, then, else_) => {
                if let Value::CBool(v) = values[*temp] {
                    let to_block_id = if v { then } else { else_ };
                    lift_to_run(*to_block_id, &blocks);
                } else {
                    lift_to_run(*then, &blocks);
                    lift_to_run(*else_, &blocks);
                }
            }
            Jump::JumpSwitchInt(temp, table, default) => {
                if let Value::CInt(i) = values[*temp] {
                    if let Some((_, to_block_id)) = table.iter().find(|(x, _)| *x == i) {
                        lift_to_run(*to_block_id, &blocks);
                    } else {
                        lift_to_run(*default, &blocks);
                    }
                } else {
                    table
                        .into_iter()
                        .for_each(|(_, to_block_id)| lift_to_run(*to_block_id, &blocks));
                    lift_to_run(*default, &blocks);
                }
            }
            Jump::JumpSwitchStr(_, table, default) => {
                table
                    .into_iter()
                    .for_each(|(_, to_block_id)| lift_to_run(*to_block_id, &blocks));
                lift_to_run(*default, &blocks);
            }
            Jump::JumpSwitchChar(temp, table, default) => {
                if let Value::CChar(i) = values[*temp] {
                    if let Some((_, to_block_id)) = table.iter().find(|(x, _)| *x == i) {
                        lift_to_run(*to_block_id, &blocks);
                    } else {
                        lift_to_run(*default, &blocks);
                    }
                } else {
                    table
                        .into_iter()
                        .for_each(|(_, to_block_id)| lift_to_run(*to_block_id, &blocks));
                    lift_to_run(*default, &blocks);
                }
            }
            Jump::Jump(to_block_id) => lift_to_run(*to_block_id, &blocks),
            _ => {}
        }
    }

    fn realize_value(blocks: &mut ssa::SsaBlockHeap, def_site: &Site, temp: TempId, value: Value) {
        if let Some(inst) = value.inst(temp) {
            match def_site {
                Site::Phi(block_id, phi_idx) => {
                    blocks[*block_id].phis.remove(*phi_idx);
                    blocks[*block_id].body.push_front(inst);
                }
                Site::Inst(block_id, inst_idx) => {
                    blocks[*block_id].body[*inst_idx] = inst;
                }
                Site::Exit(_) => unreachable!(),
            }
        }
    }

    fn realize_jump(
        blocks: &mut ssa::SsaBlockHeap,
        block_id: BlockId,
        block_run: &lir::BlockIdVec<bool>,
    ) {
        if blocks[block_id]
            .exit
            .successors()
            .filter(|b| block_run[*b])
            .count()
            == 1
        {
            let to_block_id = blocks[block_id]
                .exit
                .successors()
                .filter(|b| block_run[*b])
                .next()
                .unwrap();
            blocks[block_id].exit = lir::Jump::Jump(to_block_id);
        }
    }

    pub fn run(
        blocks: &mut ssa::SsaBlockHeap,
        block_run: &mut lir::BlockIdVec<bool>,
        entry: BlockId,
        temp_cnt: usize,
    ) {
        let mut values = lir::TempIdVec::repeat(Value::Bottom, temp_cnt);
        block_run.iter_mut().for_each(|r| *r = false);
        block_run[entry] = true;

        let mut temps_worklist = HashSet::new();
        let mut blocks_worklist = HashSet::new();

        let (uses, defs) = super::collect_uses_defs(blocks, temp_cnt);

        // Init values: Those not defined are parameters, therefore are top
        for temp in defs.iter_index() {
            if let Some(def_site) = defs[temp] {
                update_value(blocks, temp, def_site, &mut values);
            } else {
                values[temp] = Value::Top;
            }
        }

        for block_id in blocks.iter_id() {
            blocks_worklist.insert(block_id);
        }
        for temp in (0..temp_cnt).map(|i| TempId::from(i)) {
            temps_worklist.insert(temp);
        }

        while !temps_worklist.is_empty() || !blocks_worklist.is_empty() {
            while let Some(block_id) = blocks_worklist.iter().next().copied() {
                blocks_worklist.remove(&block_id);

                if !block_run[block_id] {
                    continue;
                }

                for phi_idx in 0..blocks[block_id].phis.len() {
                    process_phi(
                        blocks,
                        block_id,
                        phi_idx,
                        &mut values,
                        &block_run,
                        &mut temps_worklist,
                    )
                }

                for inst_idx in 0..blocks[block_id].body.len() {
                    process_inst(
                        blocks,
                        block_id,
                        inst_idx,
                        &mut values,
                        &block_run,
                        &mut temps_worklist,
                    );
                }

                process_jump(blocks, block_id, &values, block_run, &mut blocks_worklist);
            }

            while let Some(temp) = temps_worklist.iter().next().copied() {
                temps_worklist.remove(&temp);

                for use_site in uses[temp].iter() {
                    match use_site {
                        Site::Phi(block_id, idx) if block_run[*block_id] => {
                            process_phi(
                                blocks,
                                *block_id,
                                *idx,
                                &mut values,
                                &block_run,
                                &mut temps_worklist,
                            );
                        }
                        Site::Inst(block_id, idx) if block_run[*block_id] => {
                            process_inst(
                                blocks,
                                *block_id,
                                *idx,
                                &mut values,
                                &block_run,
                                &mut temps_worklist,
                            );
                        }
                        Site::Exit(block_id) => {
                            process_jump(
                                blocks,
                                *block_id,
                                &values,
                                block_run,
                                &mut blocks_worklist,
                            );
                        }
                        _ => {}
                    }
                }
            }
        }

        for temp in (0..temp_cnt).map(|i| TempId::from(i)) {
            if let Some(def_site) = defs[temp] {
                realize_value(blocks, &def_site, temp, values[temp]);
            }
        }
        for block_id in blocks.iter_id() {
            realize_jump(blocks, block_id, &block_run);
        }
    }
}
pub use constant_propagation::run as constant_propagation;

mod control_flow_simplification {
    use crate::lir::{self, BlockTopology};
    use crate::lir::{BlockId, BlockIdVec, Jump};
    use lir::ssa;

    fn simplify_block(
        blocks: &mut lir::BlockHeap,
        block_id: BlockId,
        block_run: &mut BlockIdVec<bool>,
        entry: &mut BlockId,
    ) -> bool {
        let mut changed = false;

        let option_first_susc = blocks[block_id].successors().next();
        if let Some(first_susc) = option_first_susc {
            if !matches!(blocks[block_id].exit, Some(Jump::Jump(_)))
                && blocks[block_id].successors().all(|b| b == first_susc)
                && first_susc != block_id
            {
                blocks[block_id].exit = Some(Jump::Jump(first_susc));
                changed = true;
            }
        }

        if let Some(Jump::Jump(only_susc)) = blocks[block_id].exit {
            if only_susc == block_id {
                return changed;
            }

            if blocks[block_id].body.is_empty() {
                for pred in blocks[block_id]
                    .pred
                    .clone()
                    .into_iter()
                    .filter(|b| block_run[*b])
                {
                    blocks[pred]
                        .successors_mut()
                        .filter(|s| **s == block_id)
                        .for_each(|s| {
                            *s = only_susc;
                        });
                    blocks[only_susc].pred.insert(pred);
                }
                blocks[only_susc].pred.remove(&block_id);
                block_run[block_id] = false;

                if block_id == *entry {
                    *entry = only_susc;
                }

                return true;
            }

            if blocks[only_susc]
                .pred
                .iter()
                .filter(|b| block_run[**b])
                .count()
                == 1
                && only_susc != *entry
            {
                blocks[only_susc]
                    .successors()
                    .collect::<Vec<_>>()
                    .into_iter()
                    .for_each(|susc_susc| {
                        blocks[susc_susc].pred.remove(&only_susc);
                        blocks[susc_susc].pred.insert(block_id);
                    });

                let only_susc_block = std::mem::take(&mut blocks[only_susc]);
                blocks[block_id]
                    .body
                    .extend(only_susc_block.body.into_iter());
                blocks[block_id].exit = only_susc_block.exit;

                block_run[only_susc] = false;

                return true;
            }

            if blocks[only_susc].body.is_empty() {
                blocks[block_id].exit = blocks[only_susc].exit.clone();

                blocks[only_susc]
                    .successors()
                    .collect::<Vec<_>>()
                    .into_iter()
                    .for_each(|susc_susc| {
                        blocks[susc_susc].pred.insert(block_id);
                    });

                changed = true;
            }
        }

        changed
    }

    pub fn run(blocks: &mut lir::BlockHeap, entry: &mut BlockId, block_run: &mut BlockIdVec<bool>) {
        loop {
            let post_order = ssa::BlocksOrder::post_order_of(blocks, *entry);

            let mut changed = false;
            for block_id in post_order.iter() {
                if block_run[block_id] {
                    changed |= simplify_block(blocks, block_id, block_run, entry);
                }
            }

            if !changed {
                break;
            }
        }

        for block_id in blocks.iter_id() {
            if let Some(Jump::Ret(Some(ret_temp))) = blocks[block_id].exit {
                if let Some(lir::Instruction::Mov(to, from)) = blocks[block_id].body.last().cloned()
                {
                    if blocks[block_id]
                        .body
                        .iter()
                        .rev()
                        .skip(1)
                        .find(|inst| inst.def() == from)
                        .is_some()
                        && to == ret_temp
                    {
                        blocks[block_id].body.pop_back().unwrap();

                        for inst in blocks[block_id].body.iter_mut().rev() {
                            if inst.def() == from {
                                *inst.def_mut() = ret_temp;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}
pub use control_flow_simplification::run as control_flow_simplification;
