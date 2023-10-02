use std::collections::HashMap;

use crate::errors;
use crate::ir;

use helo_runtime::byte_code;
use helo_runtime::executable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TempId(pub(crate) usize);

pub trait TempSubstitution {
    fn subs(&self, id: TempId) -> TempId;
}

pub struct TempSubstitutionMap(Vec<usize>);

impl TempSubstitution for TempSubstitutionMap {
    fn subs(&self, id: TempId) -> TempId {
        self.0[id.0].into()
    }
}

impl<T> TempSubstitution for T
where
    T: Fn(TempId) -> TempId,
{
    fn subs(&self, id: TempId) -> TempId {
        self(id)
    }
}

impl From<Vec<usize>> for TempSubstitutionMap {
    fn from(value: Vec<usize>) -> Self {
        Self(value)
    }
}

impl From<usize> for TempId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl TempId {
    pub fn register(self) -> byte_code::RegisterId {
        if self.0 <= u8::MAX as usize {
            byte_code::RegisterId::from(self.0 as u8)
        } else {
            panic!("TempId out of range of 256")
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockId(pub(crate) usize);
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct FunctionId(pub(crate) usize);

pub use helo_runtime::builtins::{BuiltinId, Builtins};

use ir::StrId;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Instruction {
    /// Depending on the tag code `x` in .0, take the `x`th block
    JumpTable(TempId, Vec<BlockId>),
    /// Jump denpending on .0 is true or false
    JumpIf(TempId, BlockId),
    JumpIfElse(TempId, BlockId, BlockId),
    /// Jump denpending on immediate equivalence
    JumpIfEqInt(TempId, i64, BlockId),
    JumpIfEqStr(TempId, StrId, BlockId),
    JumpIfEqBool(TempId, bool, BlockId),
    /// Jump with no condition
    Jump(BlockId),
    /// Apply .2 to .1 and store result to .0
    Apply(TempId, TempId, Vec<TempId>),
    /// Tail call
    TailCall(TempId, TempId, Vec<TempId>),
    /// Tail call user function directly
    TailCallU(TempId, FunctionId, Vec<TempId>),
    /// Call builtin
    CallBuiltin(TempId, BuiltinId, Vec<TempId>),
    /// Call user defined function
    Call(TempId, FunctionId, Vec<TempId>),
    /// Load immediate to register .0
    Int(TempId, i64),
    Float(TempId, String),
    Bool(TempId, bool),
    Str(TempId, StrId),
    /// Push .2's to closure (at .1)'s environment and store result to .0
    Push(TempId, TempId, Vec<TempId>),
    /// Loat function
    Function(TempId, FunctionId),
    /// Load builtin
    Buitltin(TempId, BuiltinId),
    /// Get field of tuple, variant at .1 and store at .0
    Field(TempId, TempId, usize),
    /// Panic with constant string
    Panic(StrId),
    /// Make a tagged variant
    Tagged(TempId, u8, Vec<TempId>),
    /// Move .1 to .0
    Mov(TempId, TempId),
    /// Return
    Ret(TempId),
}

impl Instruction {
    pub fn functional(&self) -> bool {
        use Instruction::*;
        match self {
            Jump(_)
            | JumpIf(_, _)
            | JumpIfElse(_, _, _)
            | JumpIfEqBool(_, _, _)
            | JumpIfEqInt(_, _, _)
            | JumpIfEqStr(_, _, _)
            | JumpTable(_, _) => false,
            Panic(_) | Ret(_) => false,
            Apply(_, _, _)
            | Call(_, _, _)
            | TailCall(_, _, _)
            | TailCallU(_, _, _)
            | CallBuiltin(_, _, _)
            | Int(_, _)
            | Float(_, _)
            | Bool(_, _)
            | Str(_, _)
            | Push(_, _, _)
            | Function(_, _)
            | Buitltin(_, _)
            | Field(_, _, _)
            | Tagged(_, _, _)
            | Mov(_, _) => true,
        }
    }
    pub fn output(&self) -> Option<TempId> {
        use Instruction::*;
        match self {
            Jump(_)
            | JumpIf(_, _)
            | JumpIfElse(_, _, _)
            | JumpIfEqBool(_, _, _)
            | JumpIfEqInt(_, _, _)
            | JumpIfEqStr(_, _, _)
            | JumpTable(_, _) => None,
            Call(out, _, _)
            | Apply(out, _, _)
            | TailCall(out, _, _)
            | TailCallU(out, _, _)
            | CallBuiltin(out, _, _) => Some(*out),
            Int(out, _) | Float(out, _) | Bool(out, _) | Str(out, _) => Some(*out),
            Push(out, _, _) => Some(*out),
            Function(out, _) | Buitltin(out, _) => Some(*out),
            Field(out, _, _) => Some(*out),
            Panic(_) | Ret(_) => None,
            Tagged(out, _, _) => Some(*out),
            Mov(out, _) => Some(*out),
        }
    }
    pub fn input<'a>(&'a self) -> Box<dyn Iterator<Item = TempId> + 'a> {
        use Instruction::*;
        match self {
            Jump(_)
            | Int(_, _)
            | Float(_, _)
            | Bool(_, _)
            | Str(_, _)
            | Function(_, _)
            | Buitltin(_, _)
            | Panic(_)
            | Ret(_)
            | JumpTable(_, _) => Box::new([].iter().copied()),
            Apply(_, a, args) | TailCall(_, a, args) | Push(_, a, args) => {
                Box::new([a].into_iter().copied().chain(args.iter().copied()))
            }
            Call(_, _, args)
            | TailCallU(_, _, args)
            | CallBuiltin(_, _, args)
            | Tagged(_, _, args) => Box::new(args.iter().copied()),
            Field(_, input, _)
            | Mov(_, input)
            | JumpIf(input, _)
            | JumpIfElse(input, _, _)
            | JumpIfEqBool(input, _, _)
            | JumpIfEqInt(input, _, _)
            | JumpIfEqStr(input, _, _) => Box::new([*input].into_iter()),
        }
    }
    pub fn execute_substitution(&mut self, subs: &impl TempSubstitution) {
        self.execute_substite_args(subs);
        self.execute_substite_output(subs);
    }
    pub fn execute_substite_args(&mut self, subs: &impl TempSubstitution) {
        use Instruction::*;
        match self {
            Jump(_) | Panic(_) => {}
            Int(_, _)
            | Float(_, _)
            | Bool(_, _)
            | Str(_, _)
            | Function(_, _)
            | Buitltin(_, _)
            | JumpTable(_, _) => {}
            Apply(_r, a, args) | TailCall(_r, a, args) | Push(_r, a, args) => {
                *a = subs.subs(*a);
                args.iter_mut().for_each(|arg| *arg = subs.subs(*arg));
            }
            Call(_r, _, args)
            | Tagged(_r, _, args)
            | TailCallU(_r, _, args)
            | CallBuiltin(_r, _, args) => {
                args.iter_mut().for_each(|arg| *arg = subs.subs(*arg));
            }
            Ret(input)
            | Field(_, input, _)
            | Mov(_, input)
            | JumpIf(input, _)
            | JumpIfElse(input, _, _)
            | JumpIfEqBool(input, _, _)
            | JumpIfEqInt(input, _, _)
            | JumpIfEqStr(input, _, _) => *input = subs.subs(*input),
        }
    }
    pub fn execute_substite_output(&mut self, subs: &impl TempSubstitution) {
        use Instruction::*;
        match self {
            Jump(_) | Panic(_) => {}
            JumpIf(_, _)
            | JumpIfElse(_, _, _)
            | JumpIfEqBool(_, _, _)
            | JumpIfEqInt(_, _, _)
            | JumpIfEqStr(_, _, _)
            | Int(_, _)
            | Float(_, _)
            | Bool(_, _)
            | Str(_, _)
            | Function(_, _)
            | Buitltin(_, _)
            | Ret(_)
            | JumpTable(_, _) => {}
            Apply(r, _, _args)
            | Call(r, _, _args)
            | TailCall(r, _, _args)
            | TailCallU(r, _, _args)
            | CallBuiltin(r, _, _args)
            | Push(r, _, _args)
            | Tagged(r, _, _args) => {
                *r = subs.subs(*r);
            }
            Field(r, _input, _) | Mov(r, _input) => *r = subs.subs(*r),
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
impl std::ops::IndexMut<usize> for Block {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
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
    pub fn iter(&self) -> impl Iterator<Item = &Instruction> {
        self.0.iter()
    }
    pub fn remove(&mut self, idx: usize) {
        self.0.remove(idx);
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
    pub fn execute_substitution(&mut self, subs: &impl TempSubstitution) {
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
impl std::ops::IndexMut<BlockId> for BlockHeap {
    fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}

pub struct Function {
    pub body: BlockId,
    pub blocks: BlockHeap,
    pub arity: usize,
    pub meta: helo_parse::ast::Meta,
    pub name: ir::StrId,
}

pub struct FunctionTable {
    tab: HashMap<ir::FunctionId, FunctionId>,
    store: Vec<Function>,
}

impl FunctionTable {
    pub fn new() -> Self {
        Self {
            tab: HashMap::new(),
            store: Vec::new(),
        }
    }
    pub fn get(&self, id: &ir::FunctionId) -> Option<FunctionId> {
        self.tab.get(id).map(|i| *i)
    }
    pub fn insert(&mut self, fid: ir::FunctionId, f: Function) -> FunctionId {
        let id = FunctionId(self.store.len());
        self.store.push(f);
        self.tab.insert(fid, id);
        id
    }
    pub fn to_list(self) -> Result<FunctionList, errors::MainNotFound> {
        self.tab
            .get("main")
            .map_or(Err(errors::MainNotFound {}), |main_id| {
                Ok(FunctionList {
                    v: self.store,
                    main_id: *main_id,
                })
            })
    }
}

pub struct FunctionList {
    v: Vec<Function>,
    main_id: FunctionId,
}

impl FunctionList {
    pub fn get(&self, id: FunctionId) -> Option<&Function> {
        self.v.get(id.0)
    }
    pub fn main_id(&self) -> FunctionId {
        self.main_id
    }
}

pub struct StrIndex(Vec<byte_code::StrAddr>);

impl StrIndex {
    pub fn new(list: ir::StrList) -> (Self, executable::StrChunk) {
        let mut chunk = executable::StrChunk::new();
        let index = list.iter().map(|s| chunk.push(s).unwrap()).collect();
        (Self(index), chunk)
    }
}

impl std::ops::Index<StrId> for StrIndex {
    type Output = byte_code::StrAddr;
    fn index(&self, index: StrId) -> &Self::Output {
        &self.0[index.0]
    }
}
