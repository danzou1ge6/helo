use std::collections::HashMap;
use std::collections::HashSet;
use std::marker::PhantomData;

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

pub use ir::{BuiltinId, StrId};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Instruction {
    /// Apply .2 to .1 and store result to .0
    Apply(TempId, TempId, Vec<TempId>),
    ApplyImpure(TempId, TempId, Vec<TempId>),
    /// Call builtin
    CallBuiltin(TempId, BuiltinId, Vec<TempId>),
    CallBuiltinImpure(TempId, BuiltinId, Vec<TempId>),
    /// Call user defined function
    Call(TempId, FunctionId, Vec<TempId>),
    CallImpure(TempId, FunctionId, Vec<TempId>),
    /// Call .1 with arguments .2, and store result to.0.
    /// Different from `Apply`, this instruction also implies that .1 is actually the executing closure.
    /// This information is useful for tail recursion optimization
    CallThisClosure(TempId, TempId, Vec<TempId>),
    CallThisClosureImpure(TempId, TempId, Vec<TempId>),
    /// Push values at .2 to .1. Unlike [`Apply`], this instruction loads a function and then mutates its environment.
    /// This makes recursive closure possible.
    AddToEnv(TempId, TempId, Vec<TempId>),
    /// Load immediate to register .0
    Int(TempId, i64),
    Float(TempId, String),
    Bool(TempId, bool),
    Str(TempId, StrId),
    Char(TempId, char),
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
    JumpSwitchChar(TempId, Vec<(char, BlockId)>, BlockId),
    /// Unconditional jump
    Jump(BlockId),
    /// Return from function
    Ret(Option<TempId>),
    /// Panic
    Panic {
        file: ir::StrId,
        span: (usize, usize),
        msg: ir::StrId,
    },
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
            Jump::JumpSwitchChar(_, v, default) => Box::new(
                v.iter()
                    .map(|(_, to)| to)
                    .copied()
                    .chain([*default].into_iter()),
            ),
            Jump::Jump(to) => Box::new([*to].into_iter()),
            Jump::Panic { .. } | Jump::Ret(_) => Box::new([].into_iter()),
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
            Jump::JumpSwitchChar(_, v, default) => {
                Box::new(v.iter_mut().map(|(_, to)| to).chain([default].into_iter()))
            }
            Jump::Jump(to) => Box::new([to].into_iter()),
            Jump::Panic { .. } | Jump::Ret(_) => Box::new([].into_iter()),
        }
    }
    pub fn uses(&self) -> Option<TempId> {
        use self::Jump::*;
        match self {
            JumpIfElse(r, _, _)
            | JumpSwitchInt(r, _, _)
            | JumpSwitchStr(r, _, _)
            | JumpSwitchChar(r, _, _)
            | JumpTable(r, _) => Some(*r),
            Jump(_) | Panic { .. } => None,
            Ret(r) => *r,
        }
    }
    pub fn use_mut(&mut self) -> Option<&mut TempId> {
        use self::Jump::*;
        match self {
            JumpIfElse(r, _, _)
            | JumpSwitchInt(r, _, _)
            | JumpSwitchStr(r, _, _)
            | JumpSwitchChar(r, _, _)
            | JumpTable(r, _) => Some(r),
            Jump(_) | Panic { .. } => None,
            Ret(r) => r.as_mut(),
        }
    }
}

impl Instruction {
    pub fn functional(&self) -> bool {
        use Instruction::*;
        match self {
            Apply(_, _, _)
            | Call(_, _, _)
            | CallThisClosure(_, _, _)
            | CallBuiltin(_, _, _)
            | Int(_, _)
            | Float(_, _)
            | Bool(_, _)
            | Str(_, _)
            | Char(_, _)
            | Push(_, _, _)
            | Function(_, _)
            | Buitltin(_, _)
            | Field(_, _, _)
            | AddToEnv(_, _, _)
            | Tagged(_, _, _)
            | Mov(_, _) => true,
            CallImpure(..)
            | ApplyImpure(..)
            | CallBuiltinImpure(..)
            | CallThisClosureImpure(..) => false,
        }
    }
    pub fn def(&self) -> TempId {
        use Instruction::*;
        match self {
            Call(out, _, _)
            | Apply(out, _, _)
            | CallThisClosure(out, _, _)
            | CallBuiltin(out, _, _)
            | CallImpure(out, _, _)
            | ApplyImpure(out, _, _)
            | CallThisClosureImpure(out, _, _)
            | CallBuiltinImpure(out, _, _)
            | Int(out, _)
            | Float(out, _)
            | Bool(out, _)
            | Str(out, _)
            | Char(out, _)
            | AddToEnv(out, _, _)
            | Push(out, _, _)
            | Function(out, _)
            | Buitltin(out, _)
            | Field(out, _, _)
            | Tagged(out, _, _)
            | Mov(out, _) => *out,
        }
    }
    pub fn def_mut(&mut self) -> &mut TempId {
        use Instruction::*;
        match self {
            Call(out, _, _)
            | Apply(out, _, _)
            | CallThisClosure(out, _, _)
            | CallBuiltin(out, _, _)
            | CallImpure(out, _, _)
            | ApplyImpure(out, _, _)
            | CallThisClosureImpure(out, _, _)
            | CallBuiltinImpure(out, _, _)
            | Int(out, _)
            | Float(out, _)
            | Bool(out, _)
            | Str(out, _)
            | Char(out, _)
            | AddToEnv(out, _, _)
            | Push(out, _, _)
            | Function(out, _)
            | Buitltin(out, _)
            | Field(out, _, _)
            | Tagged(out, _, _)
            | Mov(out, _) => out,
        }
    }
    pub fn uses<'a>(&'a self) -> Box<dyn Iterator<Item = TempId> + 'a> {
        use Instruction::*;
        match self {
            Int(_, _)
            | Float(_, _)
            | Bool(_, _)
            | Str(_, _)
            | Function(_, _)
            | Buitltin(_, _)
            | Char(_, _) => Box::new([].into_iter()),
            Apply(_, a, args)
            | ApplyImpure(_, a, args)
            | Push(_, a, args)
            | CallThisClosure(_, a, args)
            | CallThisClosureImpure(_, a, args)
            | AddToEnv(_, a, args) => {
                Box::new([a].into_iter().copied().chain(args.iter().copied()))
            }
            Call(_, _, args)
            | CallBuiltin(_, _, args)
            | CallImpure(_, _, args)
            | CallBuiltinImpure(_, _, args)
            | Tagged(_, _, args) => Box::new(args.iter().copied()),
            Field(_, input, _) | Mov(_, input) => Box::new([*input].into_iter()),
        }
    }
    pub fn uses_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &mut TempId> + 'a> {
        use Instruction::*;
        match self {
            Int(_, _)
            | Float(_, _)
            | Bool(_, _)
            | Str(_, _)
            | Function(_, _)
            | Buitltin(_, _)
            | Char(_, _) => Box::new([].into_iter()),
            Apply(_, a, args)
            | ApplyImpure(_, a, args)
            | Push(_, a, args)
            | CallThisClosure(_, a, args)
            | CallThisClosureImpure(_, a, args)
            | AddToEnv(_, a, args) => Box::new([a].into_iter().chain(args.iter_mut())),
            Call(_, _, args)
            | CallBuiltin(_, _, args)
            | CallImpure(_, _, args)
            | CallBuiltinImpure(_, _, args)
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

#[derive(Clone)]
pub struct AltIdxVec<T, I>(Vec<T>, PhantomData<I>);


impl<T, I> std::fmt::Debug for AltIdxVec<T, I> where T: std::fmt::Debug, I: std::fmt::Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter().enumerate()).finish()
    }
}

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
    pub fn iter_index(&self) -> impl Iterator<Item = I>
    where
        I: From<usize>,
    {
        (0..self.len()).map(|i| i.into())
    }
}

impl<T, I> FromIterator<T> for AltIdxVec<T, I> {
    fn from_iter<F: IntoIterator<Item = T>>(iter: F) -> Self {
        Self(iter.into_iter().collect(), PhantomData)
    }
}

#[derive(Debug, Default)]
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

pub trait BlockTopology {
    fn successors<'a>(&'a self) -> Box<dyn Iterator<Item = BlockId> + 'a>;
    fn predecessors<'a>(&'a self) -> impl Iterator<Item = BlockId> + 'a;
    fn successors_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut BlockId> + 'a>;
}

impl BlockTopology for Block {
    fn successors<'a>(&'a self) -> Box<dyn Iterator<Item = BlockId> + 'a> {
        match &self.exit {
            None => panic!("block not sealed, thus has no successors"),
            Some(jump) => jump.successors(),
        }
    }
    fn predecessors<'a>(&'a self) -> impl Iterator<Item = BlockId> + 'a {
        self.pred.iter().copied()
    }
    fn successors_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut BlockId> + 'a> {
        match &mut self.exit {
            None => panic!("block not sealed, thus has no successors"),
            Some(jump) => jump.successors_mut(),
        }
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
    pub fn exit(&self) -> &Jump {
        self.exit.as_ref().expect("this block is not sealed yet")
    }
    pub fn sealed(&self) -> bool {
        self.exit.is_some()
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

#[derive(Debug)]
pub struct FunctionOptimized {
    pub body: BlockId,
    pub blocks: BlockHeap,
    pub arity: usize,
    pub meta: helo_parse::ast::Meta,
    pub name: ir::StrId,
    pub temp_cnt: usize,
    pub block_run: BlockIdVec<bool>,
}

pub struct FunctionTable<'s, F> {
    tab: HashMap<ir::FunctionId<'s>, FunctionId>,
    store: Vec<Option<F>>,
}

impl<'s, F> FunctionTable<'s, F> {
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
        fid: ir::FunctionId<'s>,
        gen: impl FnOnce(FunctionId, &mut FunctionTable<'s, F>) -> F,
    ) -> FunctionId {
        let id = FunctionId(self.store.len());
        self.store.push(None);
        self.tab.insert(fid, id);

        let f = gen(id, self);
        self.store[id.0] = Some(f);

        id
    }
    pub fn to_list(self) -> FunctionList<F> {
        FunctionList {
            v: self.store.into_iter().map(|x| x.unwrap()).collect(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionList<F> {
    v: Vec<F>,
}

impl<F> FunctionList<F> {
    pub fn get(&self, id: FunctionId) -> Option<&F> {
        self.v.get(id.0)
    }
    pub fn get_mut(&mut self, id: FunctionId) -> Option<&mut F> {
        self.v.get_mut(id.0)
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut F> {
        self.v.iter_mut()
    }
    pub fn iter_id(&self) -> impl Iterator<Item = FunctionId> {
        (0..self.v.len()).map(|i| FunctionId(i))
    }
    pub fn into_iter(self) -> impl Iterator<Item = F> {
        self.v.into_iter()
    }
    pub fn into_iter_id(self) -> impl Iterator<Item = (FunctionId, F)> {
        self.v
            .into_iter()
            .enumerate()
            .map(|(i, f)| (FunctionId(i), f))
    }
    pub fn push(&mut self, f: F) {
        self.v.push(f);
    }
    pub fn new() -> Self {
        Self { v: Vec::new() }
    }
}

impl FunctionList<Function> {
    pub fn function_name_list(&self) -> FunctionNameList {
        let v = self.v.iter().map(|f| f.name).collect();
        FunctionNameList { v }
    }
}
impl FunctionList<FunctionOptimized> {
    pub fn function_name_list(&self) -> FunctionNameList {
        let v = self.v.iter().map(|f| f.name).collect();
        FunctionNameList { v }
    }
}

impl<F> FromIterator<F> for FunctionList<F> {
    fn from_iter<T: IntoIterator<Item = F>>(iter: T) -> Self {
        let v = iter.into_iter().collect();
        Self { v }
    }
}

pub struct FunctionNameList {
    v: Vec<StrId>,
}

impl FunctionNameList {
    pub fn get(&self, id: FunctionId) -> StrId {
        self.v[id.0]
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
