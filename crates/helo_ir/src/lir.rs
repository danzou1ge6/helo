use std::collections::HashMap;

use crate::ir;

#[derive(Debug, Clone, Copy)]
pub struct TempId(pub usize);

impl TempId {}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy)]
pub struct FunctionId(pub usize);

pub use helo_runtime::builtins::{BuiltinId, Builtins};

use ir::StrId;

pub enum Instruction {
    /// Depending on the tag code `x` in .0, take the `x`th [`Jump`] instruction that follows
    JunpTable(TempId),
    /// Jump denpending on .0 is true or false
    JumpIf(TempId, BlockId),
    /// Jump denpending on immediate equivalence
    JumpIfEqInt(TempId, i64, BlockId),
    JumpIfEqStr(TempId, ir::StrId, BlockId),
    JumpIfEqBool(TempId, bool, BlockId),
    /// Jump with no condition
    Jump(BlockId),
    /// Apply arguments stored at
    Apply {
        ret: TempId,
        callee: TempId,
        args: Vec<TempId>,
    },
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
    /// Call builtin
    CallBuiltin(TempId, BuiltinId, Vec<TempId>),
    /// Get field of tuple, variant at .1 and store at .0
    Field(TempId, TempId, usize),
    /// Panic with constant string
    Panic(StrId),
    /// Make a tagged variant
    Tagged(TempId, usize, Vec<TempId>),
    /// Move .1 to .0
    Mov(TempId, TempId),
}

pub struct Block(Vec<Instruction>);

impl Block {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn push(&mut self, inst: Instruction) {
        self.0.push(inst)
    }
}

pub struct BlockHeap(Vec<Block>);

impl BlockHeap {
    pub fn push(&mut self, block: Block) -> BlockId {
        let id = BlockId(self.0.len());
        self.0.push(block);
        id
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
