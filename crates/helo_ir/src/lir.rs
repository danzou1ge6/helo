use std::collections::HashMap;

use crate::ir;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TempId(pub(crate) usize);

pub struct TempSubstitution(Vec<usize>);

impl TempSubstitution {
    pub fn subs(&self, id: TempId) -> TempId {
        self.0[id.0].into()
    }
}

impl From<Vec<usize>> for TempSubstitution {
    fn from(value: Vec<usize>) -> Self {
        Self(value)
    }
}

impl From<usize> for TempId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl TempId {}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(pub(crate) usize);
#[derive(Debug, Clone, Copy)]
pub struct FunctionId(pub(crate) usize);

pub use helo_runtime::builtins::{BuiltinId, Builtins};

use ir::StrId;

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Depending on the tag code `x` in .0, take the `x`th block
    JumpTable(TempId, Vec<BlockId>),
    /// Jump denpending on .0 is true or false
    JumpIf(TempId, BlockId),
    /// Jump denpending on immediate equivalence
    JumpIfEqInt(TempId, i64, BlockId),
    JumpIfEqStr(TempId, ir::StrId, BlockId),
    JumpIfEqBool(TempId, bool, BlockId),
    /// Jump with no condition
    Jump(BlockId),
    /// Apply arguments stored at
    Apply(TempId, TempId, Vec<TempId>),
    /// Tail call
    TailCall(TempId, TempId, Vec<TempId>),
    /// Call builtin
    CallBuiltin(TempId, BuiltinId, Vec<TempId>),
    /// Load immediate to register .0
    Int(TempId, i64),
    Float(TempId, f64),
    Bool(TempId, bool),
    Str(TempId, StrId),
    /// Push .1's to closure, variant or tuple at .0
    Push(TempId, Vec<TempId>),
    /// Loat function
    Function(TempId, FunctionId),
    /// Load builtin
    Buitltin(TempId, BuiltinId),
    /// Get field of tuple, variant at .1 and store at .0
    Field(TempId, TempId, usize),
    /// Panic with constant string
    Panic(StrId),
    /// Make a tagged variant
    Tagged(TempId, usize, Vec<TempId>),
    /// Move .1 to .0
    Mov(TempId, TempId),
}

impl Instruction {
    pub fn output(&self) -> Option<TempId> {
        use Instruction::*;
        match self {
            Jump(_)
            | JumpIf(_, _)
            | JumpIfEqBool(_, _, _)
            | JumpIfEqInt(_, _, _)
            | JumpIfEqStr(_, _, _)
            | JumpTable(_, _) => None,
            Apply(out, _, _) | TailCall(out, _, _) | CallBuiltin(out, _, _) => Some(*out),
            Int(out, _) | Float(out, _) | Bool(out, _) | Str(out, _) => Some(*out),
            Push(out, _) => Some(*out),
            Function(out, _) | Buitltin(out, _) => Some(*out),
            Field(out, _, _) => Some(*out),
            Panic(_) => None,
            Tagged(out, _, _) => Some(*out),
            Mov(out, _) => Some(*out),
        }
    }
    pub fn input<'a>(&'a self) -> Box<dyn Iterator<Item = TempId> + 'a> {
        use Instruction::*;
        match self {
            Jump(_)
            | JumpIf(_, _)
            | JumpIfEqBool(_, _, _)
            | JumpIfEqInt(_, _, _)
            | JumpIfEqStr(_, _, _)
            | Int(_, _)
            | Float(_, _)
            | Bool(_, _)
            | Str(_, _)
            | Function(_, _)
            | Buitltin(_, _)
            | Panic(_)
            | JumpTable(_, _) => Box::new([].iter().copied()),
            Apply(_, _, args)
            | TailCall(_, _, args)
            | CallBuiltin(_, _, args)
            | Push(_, args)
            | Tagged(_, _, args) => Box::new(args.iter().copied()),
            Field(_, input, _) | Mov(_, input) => Box::new([*input].into_iter()),
        }
    }
    pub fn execute_substitution(&mut self, subs: &TempSubstitution) {
        use Instruction::*;
        match self {
            Jump(_) | Panic(_) => {}
            JumpIf(c, _)
            | JumpIfEqBool(c, _, _)
            | JumpIfEqInt(c, _, _)
            | JumpIfEqStr(c, _, _)
            | Int(c, _)
            | Float(c, _)
            | Bool(c, _)
            | Str(c, _)
            | Function(c, _)
            | Buitltin(c, _)
            | JumpTable(c, _) => *c = subs.subs(*c),
            Apply(r, _, args)
            | TailCall(r, _, args)
            | CallBuiltin(r, _, args)
            | Push(r, args)
            | Tagged(r, _, args) => {
                *r = subs.subs(*r);
                args.iter_mut().for_each(|arg| *arg = subs.subs(*arg));
            }
            Field(r, input, _) | Mov(r, input) => {
                *r = subs.subs(*r);
                *input = subs.subs(*input)
            }
        }
    }
}

pub struct Block(Vec<Instruction>);

impl std::ops::Index<usize> for Block {
    type Output = Instruction;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl Block {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn push(&mut self, inst: Instruction) {
        self.0.push(inst)
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Instruction> {
        self.0.iter_mut()
    }
}

pub struct BlockHeap(Vec<Block>);

impl BlockHeap {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn push(&mut self, block: Block) -> BlockId {
        let id = BlockId(self.0.len());
        self.0.push(block);
        id
    }
    pub fn iter(&self) -> impl Iterator<Item = &Block> {
        self.0.iter()
    }
    pub fn suscessive<'a>(
        &'a self,
        block_id: BlockId,
        idx: usize,
    ) -> Box<dyn Iterator<Item = (BlockId, usize)> + 'a> {
        use Instruction::*;
        match &self[block_id][idx] {
            Jump(b)
            | JumpIf(_, b)
            | JumpIfEqBool(_, _, b)
            | JumpIfEqInt(_, _, b)
            | JumpIfEqStr(_, _, b) => Box::new([(*b, 0)].into_iter()),
            JumpTable(_, branches) => Box::new(branches.iter().map(|b| (*b, 0))),
            _ => Box::new([(block_id, idx + 1)].into_iter()),
        }
    }
    pub fn execute_substitution(&mut self, subs: &TempSubstitution) {
        for b in self.0.iter_mut() {
            b.iter_mut()
                .for_each(|inst| inst.execute_substitution(subs))
        }
    }
}

impl std::ops::Index<BlockId> for BlockHeap {
    type Output = Block;
    fn index(&self, index: BlockId) -> &Self::Output {
        &self.0[index.0]
    }
}

pub struct Function {
    pub body: BlockId,
    pub blocks: BlockHeap,
    pub arity: usize,
}

pub struct FunctionTable {
    tab: HashMap<ir::FunctionId, FunctionId>,
    store: Vec<Function>,
}

impl FunctionTable {
    pub fn get(&self, id: &ir::FunctionId) -> Option<FunctionId> {
        self.tab.get(id).map(|i| *i)
    }
    pub fn insert(&mut self, fid: ir::FunctionId, f: Function) -> FunctionId {
        let id = FunctionId(self.store.len());
        self.store.push(f);
        self.tab.insert(fid, id);
        id
    }
}
