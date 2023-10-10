use std::collections::HashMap;
use std::collections::HashSet;
use std::marker::PhantomData;

use crate::errors;
use crate::ir;

use helo_runtime::byte_code;
use helo_runtime::executable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TempId(pub(crate) usize);

impl TempId {
    pub fn new(temp_cnt: &mut usize) -> Self {
        let r = TempId(*temp_cnt);
        *temp_cnt += 1;
        r
    }
}

impl From<TempId> for usize {
    fn from(value: TempId) -> Self {
        value.0
    }
}

pub type TempIdVec<T> = AltIdxVec<T, TempId>;

impl std::fmt::Display for TempId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
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

impl From<BlockId> for usize {
    fn from(value: BlockId) -> Self {
        value.0
    }
}

impl std::fmt::Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub use helo_runtime::builtins::{BuiltinId, Builtins};

use ir::StrId;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Instruction {
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
    /// Make a tagged variant
    Tagged(TempId, u8, Vec<TempId>),
    /// Move .1 to .0
    Mov(TempId, TempId),
}

#[derive(Debug, Clone)]
pub enum Jump {
    /// Depending on the tag code `x` in .0, take the `x`th block
    JumpTable(TempId, Vec<BlockId>),
    /// Jump denpending on .0 is true or false
    JumpIfElse(TempId, BlockId, BlockId),
    /// Jump denpending on immediate equivalence
    JumpSwitchInt(TempId, Vec<(i64, BlockId)>, BlockId),
    JumpSwitchStr(TempId, Vec<(StrId, BlockId)>, BlockId),
    /// Unconditional jump
    Jump(BlockId),
    /// Return from function
    Ret(TempId),
    /// Panic
    Panic(StrId),
}

impl Jump {
    pub fn successors<'a>(&'a self) -> Box<dyn Iterator<Item = BlockId> + 'a> {
        match self {
            Jump::JumpTable(_, to) => Box::new(to.iter().copied()),
            Jump::JumpIfElse(_, to1, to2) => Box::new([*to1, *to2].into_iter()),
            Jump::JumpSwitchInt(_, v, default) => Box::new(
                v.iter()
                    .map(|(_, to)| to)
                    .copied()
                    .chain([*default].into_iter()),
            ),
            Jump::JumpSwitchStr(_, v, default) => Box::new(
                v.iter()
                    .map(|(_, to)| to)
                    .copied()
                    .chain([*default].into_iter()),
            ),
            Jump::Jump(to) => Box::new([*to].into_iter()),
            Jump::Panic(_) | Jump::Ret(_) => Box::new([].into_iter()),
        }
    }
    pub fn successors_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut BlockId> + 'a> {
        match self {
            Jump::JumpTable(_, to) => Box::new(to.iter_mut()),
            Jump::JumpIfElse(_, to1, to2) => Box::new([to1, to2].into_iter()),
            Jump::JumpSwitchInt(_, v, default) => {
                Box::new(v.iter_mut().map(|(_, to)| to).chain([default].into_iter()))
            }
            Jump::JumpSwitchStr(_, v, default) => {
                Box::new(v.iter_mut().map(|(_, to)| to).chain([default].into_iter()))
            }
            Jump::Jump(to) => Box::new([to].into_iter()),
            Jump::Panic(_) | Jump::Ret(_) => Box::new([].into_iter()),
        }
    }
    pub fn uses(&self) -> Option<TempId> {
        use self::Jump::*;
        match self {
            JumpIfElse(r, _, _)
            | JumpSwitchInt(r, _, _)
            | JumpSwitchStr(r, _, _)
            | Ret(r)
            | JumpTable(r, _) => Some(*r),
            Jump(_) | Panic(_) => None,
        }
    }
    pub fn use_mut(&mut self) -> Option<&mut TempId> {
        use self::Jump::*;
        match self {
            JumpIfElse(r, _, _)
            | JumpSwitchInt(r, _, _)
            | JumpSwitchStr(r, _, _)
            | Ret(r)
            | JumpTable(r, _) => Some(r),
            Jump(_) | Panic(_) => None,
        }
    }
}

impl Instruction {
    pub fn functional(&self) -> bool {
        use Instruction::*;
        match self {
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
    pub fn def(&self) -> TempId {
        use Instruction::*;
        match self {
            Call(out, _, _)
            | Apply(out, _, _)
            | TailCall(out, _, _)
            | TailCallU(out, _, _)
            | CallBuiltin(out, _, _) => *out,
            Int(out, _) | Float(out, _) | Bool(out, _) | Str(out, _) => *out,
            Push(out, _, _) => *out,
            Function(out, _) | Buitltin(out, _) => *out,
            Field(out, _, _) => *out,
            Tagged(out, _, _) => *out,
            Mov(out, _) => *out,
        }
    }
    pub fn def_mut(&mut self) -> &mut TempId {
        use Instruction::*;
        match self {
            Call(out, _, _)
            | Apply(out, _, _)
            | TailCall(out, _, _)
            | TailCallU(out, _, _)
            | CallBuiltin(out, _, _) => out,
            Int(out, _) | Float(out, _) | Bool(out, _) | Str(out, _) => out,
            Push(out, _, _) => out,
            Function(out, _) | Buitltin(out, _) => out,
            Field(out, _, _) => out,
            Tagged(out, _, _) => out,
            Mov(out, _) => out,
        }
    }
    pub fn uses<'a>(&'a self) -> Box<dyn Iterator<Item = TempId> + 'a> {
        use Instruction::*;
        match self {
            Int(_, _) | Float(_, _) | Bool(_, _) | Str(_, _) | Function(_, _) | Buitltin(_, _) => {
                Box::new([].into_iter())
            }
            Apply(_, a, args) | TailCall(_, a, args) | Push(_, a, args) => {
                Box::new([a].into_iter().copied().chain(args.iter().copied()))
            }
            Call(_, _, args)
            | TailCallU(_, _, args)
            | CallBuiltin(_, _, args)
            | Tagged(_, _, args) => Box::new(args.iter().copied()),
            Field(_, input, _) | Mov(_, input) => Box::new([*input].into_iter()),
        }
    }
    pub fn uses_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &mut TempId> + 'a> {
        use Instruction::*;
        match self {
            Int(_, _) | Float(_, _) | Bool(_, _) | Str(_, _) | Function(_, _) | Buitltin(_, _) => {
                Box::new([].into_iter())
            }
            Apply(_, a, args) | TailCall(_, a, args) | Push(_, a, args) => {
                Box::new([a].into_iter().chain(args.iter_mut()))
            }
            Call(_, _, args)
            | TailCallU(_, _, args)
            | CallBuiltin(_, _, args)
            | Tagged(_, _, args) => Box::new(args.iter_mut()),
            Field(_, input, _) | Mov(_, input) => Box::new([input].into_iter()),
        }
    }
    pub fn substitute_temps_with(&mut self, mapping: &impl Fn(TempId) -> TempId) {
        self.uses_mut().for_each(|u| *u = mapping(*u));
        let def = self.def_mut();
        *def = mapping(*def);
    }
}

#[derive(Clone, Debug)]
pub struct AltIdxVec<T, I>(Vec<T>, PhantomData<I>);

pub type BlockIdVec<T> = AltIdxVec<T, BlockId>;

impl<T, I> std::ops::Index<I> for AltIdxVec<T, I>
where
    I: Into<usize>,
{
    type Output = T;
    fn index(&self, index: I) -> &Self::Output {
        &self.0[index.into()]
    }
}

impl<T, I> std::ops::IndexMut<I> for AltIdxVec<T, I>
where
    I: Into<usize>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.0[index.into()]
    }
}

impl<T, I> AltIdxVec<T, I> {
    pub fn new() -> Self {
        Self(Vec::new(), PhantomData)
    }
    pub fn repeat(v: T, n: usize) -> Self
    where
        T: Clone,
    {
        Self(vec![v; n], PhantomData)
    }
    pub fn push(&mut self, v: T) {
        self.0.push(v)
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a T> + 'a {
        self.0.iter()
    }
    pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut T> + 'a {
        self.0.iter_mut()
    }
    pub fn into_iter(self) -> impl Iterator<Item = T> {
        self.0.into_iter()
    }
    pub fn iter_index(&self) -> impl Iterator<Item = I> where I: From<usize> {
        (0..self.len()).map(|i| i.into())
    }
}

impl<T, I> FromIterator<T> for AltIdxVec<T, I> {
    fn from_iter<F: IntoIterator<Item = T>>(iter: F) -> Self {
        Self(iter.into_iter().collect(), PhantomData)
    }
}

#[derive(Debug)]
pub struct Block {
    body: imbl::Vector<Instruction>,
    pred: HashSet<BlockId>,
    exit: Option<Jump>,
}

impl std::ops::Index<usize> for Block {
    type Output = Instruction;
    fn index(&self, index: usize) -> &Self::Output {
        &self.body[index]
    }
}
impl std::ops::IndexMut<usize> for Block {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.body[index]
    }
}

impl Block {
    pub fn new() -> Self {
        Self {
            body: imbl::Vector::new(),
            pred: HashSet::new(),
            exit: None,
        }
    }
    pub fn push(&mut self, inst: Instruction) {
        if self.exit.is_none() {
            self.body.push_back(inst)
        }
    }
    pub fn len(&self) -> usize {
        self.body.len()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Instruction> {
        self.body.iter_mut()
    }
    pub fn iter(&self) -> impl Iterator<Item = &Instruction> {
        self.body.iter()
    }
    pub fn remove(&mut self, idx: usize) {
        self.body.remove(idx);
    }
    fn seal(&mut self, jump: Jump) {
        if self.exit.is_some() {
            panic!("block already sealed")
        }
        self.exit = Some(jump);
    }
    pub fn successors<'a>(&'a self) -> Box<dyn Iterator<Item = BlockId> + 'a> {
        match &self.exit {
            None => panic!("block not sealed, thus has no successors"),
            Some(jump) => jump.successors(),
        }
    }
    pub fn predessorss<'a>(&'a self) -> impl Iterator<Item = BlockId> + 'a {
        self.pred.iter().copied()
    }
    pub fn exit(&self) -> &Jump {
        self.exit.as_ref().expect("this block is not sealed yet")
    }
}

#[derive(Debug)]
pub struct BlockHeap_<B>(BlockIdVec<B>);

impl<B> Default for BlockHeap_<B> {
    fn default() -> Self {
        Self(BlockIdVec::new())
    }
}

pub type BlockHeap = BlockHeap_<Block>;

impl<B> BlockHeap_<B> {
    pub fn new() -> Self {
        Self(BlockIdVec::new())
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn push(&mut self, block: B) -> BlockId {
        let id = BlockId(self.0.len());
        self.0.push(block);
        id
    }
    pub fn iter(&self) -> impl Iterator<Item = &B> {
        self.0.iter()
    }
    pub fn iter_id(&self) -> impl Iterator<Item = BlockId> {
        (0..self.0.len()).map(|i| BlockId(i))
    }
    pub fn into_iter(self) -> impl Iterator<Item = B> {
        self.0.into_iter()
    }
}

impl BlockHeap {
    pub fn suscessive<'a>(&'a self, block_id: BlockId, idx: usize) -> Option<(BlockId, usize)> {
        if idx + 1 < self[block_id].len() {
            Some((block_id, idx + 1))
        } else {
            None
        }
    }
    pub fn new_block(&mut self) -> BlockId {
        self.push(Block::new())
    }
    pub fn seal(&mut self, block_id: BlockId, jump: Jump) {
        for susc in jump.successors() {
            self[susc].pred.insert(block_id);
        }
        self[block_id].seal(jump);
    }
}

impl<B> std::ops::Index<BlockId> for BlockHeap_<B> {
    type Output = B;
    fn index(&self, index: BlockId) -> &Self::Output {
        &self.0[index]
    }
}
impl<B> std::ops::IndexMut<BlockId> for BlockHeap_<B> {
    fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
        &mut self.0[index]
    }
}

#[derive(Debug)]
pub struct Function {
    pub body: BlockId,
    pub blocks: BlockHeap,
    pub arity: usize,
    pub meta: helo_parse::ast::Meta,
    pub name: ir::StrId,
    pub temp_cnt: usize,
}

pub struct FunctionTable {
    tab: HashMap<ir::FunctionId, FunctionId>,
    store: Vec<Option<Function>>,
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
    pub fn insert(
        &mut self,
        fid: ir::FunctionId,
        gen: impl FnOnce(FunctionId, &mut FunctionTable) -> Function,
    ) -> FunctionId {
        let id = FunctionId(self.store.len());
        self.store.push(None);
        self.tab.insert(fid, id);

        let f = gen(id, self);
        self.store[id.0] = Some(f);

        id
    }
    pub fn main_id(&self) -> Result<FunctionId, errors::MainNotFound> {
        self.tab.get("main")
            .map_or_else(|| Err(errors::MainNotFound {}), |x| Ok(*x))
    }
    pub fn to_list(self) -> FunctionList {
        FunctionList {
            v: self.store.into_iter().map(|x| x.unwrap()).collect(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionList {
    v: Vec<Function>,
}

impl FunctionList {
    pub fn get(&self, id: FunctionId) -> Option<&Function> {
        self.v.get(id.0)
    }
    pub fn get_mut(&mut self, id: FunctionId) -> Option<&mut Function> {
        self.v.get_mut(id.0)
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Function> {
        self.v.iter_mut()
    }
    pub fn iter_id(&self) -> impl Iterator<Item = FunctionId> {
        (0..self.v.len()).map(|i| FunctionId(i))
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

pub mod optimizations;
pub mod ssa;
