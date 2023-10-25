use num_enum::{FromPrimitive, IntoPrimitive};

use helo_macro::{ChunkReaderReadArgs, ConstStrName, Emit, ToOpCode};

/// Takes 2 bytes
use crate::builtins::BuiltinId;
/// Takes 1 bytes
#[derive(PartialEq, Eq, Clone, Copy, Default, Debug)]
pub struct RegisterId(pub(crate) u8);
/// Takes 4 bytes
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct StrAddr(pub(crate) u32);
/// Take 4 bytes
#[derive(PartialEq, Eq, Clone, Copy, Default, Debug)]
pub struct Addr(pub(crate) u32);
#[derive(PartialEq, Eq, Clone, Copy, Default)]
pub struct JumpDistance(pub(crate) i16);

impl std::fmt::Display for StrAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Display for BuiltinId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Display for RegisterId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

impl std::fmt::Display for JumpDistance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::ops::Add<u32> for Addr {
    type Output = Addr;
    fn add(self, rhs: u32) -> Self::Output {
        Addr(self.0 + rhs)
    }
}

impl std::ops::Add<JumpDistance> for Addr {
    type Output = Addr;
    fn add(self, rhs: JumpDistance) -> Self::Output {
        Addr((self.0 as i64 + rhs.0 as i64) as u32)
    }
}

impl From<Addr> for usize {
    fn from(value: Addr) -> Self {
        value.0 as usize
    }
}

impl std::ops::AddAssign<u32> for Addr {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs
    }
}

impl std::ops::AddAssign<JumpDistance> for Addr {
    fn add_assign(&mut self, rhs: JumpDistance) {
        self.0 = (self.0 as i64 + rhs.0 as i64) as u32
    }
}

impl std::fmt::Display for Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#010x}", self.0)
    }
}

impl From<u8> for RegisterId {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl From<u32> for StrAddr {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<u32> for Addr {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl JumpDistance {
    pub fn bytes_len() -> u8 {
        2
    }
}

impl TryFrom<usize> for JumpDistance {
    type Error = std::num::TryFromIntError;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        i16::try_from(value).map(|i| Self(i))
    }
}

impl From<u16> for BuiltinId {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

#[derive(Emit, ToOpCode, ChunkReaderReadArgs)]
pub enum Instruction {
    /// Following is a table of relative jump distance. The `i`th entry is taken for tag `i` at .0.
    /// The relative jump distance is the difference of address of jump target and address of this instruction.
    /// Relative jump distance takes one byte
    ///
    /// 3 of 8 bytes
    JumpTable(RegisterId, u8),
    /// Perform a relative jump if .0 is true.
    ///
    /// 4 of 8 bytes
    JumpIf(RegisterId, JumpDistance),
    /// Jumps if .0 equals .1. .1 takes 4 bytes
    ///
    /// 8 of 8 bytes
    JumpIfEqI32(RegisterId, i32, JumpDistance),
    /// Jump if .0 equals i64 stored in next 8 bytes.
    ///
    /// 4 of 8 bytes
    JumpIfEqI64(RegisterId, JumpDistance),
    /// Jump if .0 equals string at .1.
    ///
    /// 8 of 8 bytes
    JumpIfEqStr(RegisterId, StrAddr, JumpDistance),
    /// Jump if .0 equals bool at .1
    ///
    /// 5 of 8 bytes
    JumpIfEqBool(RegisterId, bool, JumpDistance),
    /// Jump if .0 equals char at .1
    ///
    /// 8 of 8 bytes
    JumpIfEqChar(RegisterId, char, JumpDistance),
    /// Jump a relative distance
    ///
    /// 3 of 8 bytes
    Jump(JumpDistance),

    /// Apply .2 to .1 and store result to .0
    ///
    /// 4 of 8 bytes
    Apply1(RegisterId, RegisterId, [RegisterId; 1]),
    /// 5 of 8 bytes
    Apply2(RegisterId, RegisterId, [RegisterId; 2]),
    /// 6 of 8 bytes
    Apply3(RegisterId, RegisterId, [RegisterId; 3]),
    /// 7 of 8 bytes
    Apply4(RegisterId, RegisterId, [RegisterId; 4]),
    /// 8 of 8 bytes
    Apply5(RegisterId, RegisterId, [RegisterId; 5]),
    /// Apply the following .2 number of register-ids to .1
    ///
    /// 4 of 8 bytes
    ApplyMany(RegisterId, RegisterId, u8),

    /// Call .1 with .2 and store result to .0
    ///
    /// 7 of 8 bytes
    Call1(RegisterId, Addr, [RegisterId; 1]),
    /// 8 of 8 bytes
    Call2(RegisterId, Addr, [RegisterId; 2]),
    /// Call .1 with following .2 number of registers
    ///
    /// 4 of 8 bytes
    CallMany(RegisterId, Addr, u8),

    /// Tail call .1 with .2
    ///
    /// 6 of 8 bytes
    TailCall1(Addr, [RegisterId; 1]),
    /// 7 of 8 bytes
    TailCall2(Addr, [RegisterId; 2]),
    /// 7 of 8 bytes
    TailCall3(Addr, [RegisterId; 3]),
    /// Tail call .1 with following .2 number of registers
    ///
    /// 4 of 8 bytes
    TailCallMany(Addr, u8),

    /// Tail call .1 with .2
    ///
    /// 3 of 8 bytes
    TailCallLocal1(RegisterId, [RegisterId; 1]),
    /// 4 of 8 bytes
    TailCallLocal2(RegisterId, [RegisterId; 2]),
    /// 5 of 8 bytes
    TailCallLocal3(RegisterId, [RegisterId; 3]),
    /// 6 of 8 bytes
    TailCallLocal4(RegisterId, [RegisterId; 4]),
    /// 7 of 8 bytes
    TailCallLocal5(RegisterId, [RegisterId; 5]),
    /// 8 of 8 bytes
    TailCallLocal6(RegisterId, [RegisterId; 7]),
    /// Call .1 with following .2 number of registers
    ///
    /// 4 of 8 bytes
    TailCallLocalMany(RegisterId, u8),

    /// call builtin .1 with .2 and store result to .0
    ///
    /// 5 of 8 bytes
    CallBuiltin1(RegisterId, BuiltinId, [RegisterId; 1]),
    /// 6 of 8 bytes
    CallBuiltin2(RegisterId, BuiltinId, [RegisterId; 2]),
    /// 7 of 8 bytes
    CallBuiltin3(RegisterId, BuiltinId, [RegisterId; 3]),
    /// 8 of 8 bytes
    CallBuiltin4(RegisterId, BuiltinId, [RegisterId; 4]),

    /// Load immediate to register .0
    ///
    /// 6 of 8 bytes
    Int32(RegisterId, i32),
    /// 3 of 8 bytes
    Bool(RegisterId, bool),
    /// 6 of 8 bytes
    Str(RegisterId, StrAddr),
    /// 6 of 8 bytes
    Char(RegisterId, char),
    /// Load Immediate at following 8-bytes to .0
    ///
    /// 2 of 8 bytes
    Int64(RegisterId),
    /// 2 of 8 bytes
    Float(RegisterId),

    /// Add .1 to closure at .0
    ///
    /// 3 of 8 bytes
    AddToEnv1(RegisterId, [RegisterId; 1]),
    /// 4 of 8 bytes
    AddToEnv2(RegisterId, [RegisterId; 2]),
    /// 5 of 8 bytes
    AddToEnv3(RegisterId, [RegisterId; 3]),
    /// 6 of 8 bytes
    AddToEnv4(RegisterId, [RegisterId; 4]),
    /// 7 of 8 bytes
    AddToEnv5(RegisterId, [RegisterId; 5]),
    /// 8 of 8 bytes
    AddToEnv6(RegisterId, [RegisterId; 6]),

    /// Push .1 to array at .0. Variants, tuples are both represented by arrays.
    ///
    /// 3 of 8 bytes
    Push1(RegisterId, [RegisterId; 1]),
    /// 4 of 8 bytes
    Push2(RegisterId, [RegisterId; 2]),
    /// 5 of 8 bytes
    Push3(RegisterId, [RegisterId; 3]),
    /// 6 of 8 bytes
    Push4(RegisterId, [RegisterId; 4]),
    /// 7 of 8 bytes
    Push5(RegisterId, [RegisterId; 5]),
    /// 8 of 8 bytes
    Push6(RegisterId, [RegisterId; 6]),

    /// Load a user function
    ///
    /// 6 of 8 bytes
    Function(RegisterId, Addr),
    /// Load a builtin
    ///
    /// 5 of 8 bytes
    Builtin(RegisterId, BuiltinId),

    /// Get filed .2 of .1 and store result at .0
    ///
    /// 4 of 8 bytes
    Field(RegisterId, RegisterId, u8),

    /// Make a variant of tag .1 and fields .2
    ///
    /// 4 of 8 bytes
    Tagged1(RegisterId, u8, [RegisterId; 1]),
    /// 5 of 8 bytes
    Tagged2(RegisterId, u8, [RegisterId; 2]),
    /// 6 of 8 bytes
    Tagged3(RegisterId, u8, [RegisterId; 3]),
    /// 7 of 8 bytes
    Tagged4(RegisterId, u8, [RegisterId; 4]),
    /// 8 of 8 bytes
    Tagged5(RegisterId, u8, [RegisterId; 5]),

    /// Make a variant with no fields
    ///
    /// 3 of 8 bytes
    Tagged(RegisterId, u8),

    /// Copy value at .1 to .0
    ///
    /// 3 of 8 bytes
    Mov(RegisterId, RegisterId),

    /// Return value at .0
    Ret(RegisterId),

    /// Panic with static string at .0
    ///
    /// 5 of 8 bytes
    Panic(StrAddr),
}

#[derive(FromPrimitive, IntoPrimitive, PartialEq, Eq, Debug, ConstStrName)]
#[repr(u8)]
pub enum OpCode {
    JUMP_TABLE,
    JUMP_IF,
    JUMP_IF_EQ_I32,
    JUMP_IF_EQ_I64,
    JUMP_IF_EQ_STR,
    JUMP_IF_EQ_BOOL,
    JUMP_IF_EQ_CHAR,
    JUMP,
    APPLY1,
    APPLY2,
    APPLY3,
    APPLY4,
    APPLY5,
    APPLY_MANY,
    CALL1,
    CALL2,
    CALL_MANY,
    TAIL_CALL1,
    TAIL_CALL2,
    TAIL_CALL3,
    TAIL_CALL_MANY,
    TAIL_CALL_LOCAL1,
    TAIL_CALL_LOCAL2,
    TAIL_CALL_LOCAL3,
    TAIL_CALL_LOCAL4,
    TAIL_CALL_LOCAL5,
    TAIL_CALL_LOCAL6,
    TAIL_CALL_LOCAL_MANY,
    CALL_BUILTIN1,
    CALL_BUILTIN2,
    CALL_BUILTIN3,
    CALL_BUILTIN4,
    INT32,
    BOOL,
    STR,
    CHAR,
    INT64,
    FLOAT,
    ADD_TO_ENV1,
    ADD_TO_ENV2,
    ADD_TO_ENV3,
    ADD_TO_ENV4,
    ADD_TO_ENV5,
    ADD_TO_ENV6,
    PUSH1,
    PUSH2,
    PUSH3,
    PUSH4,
    PUSH5,
    PUSH6,
    FUNCTION,
    BUILTIN,
    FIELD,
    TAGGED1,
    TAGGED2,
    TAGGED3,
    TAGGED4,
    TAGGED5,
    TAGGED,
    MOV,
    RET,
    PANIC,
    #[num_enum(catch_all)]
    UNKNOWN(u8),
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl OpCode {
    pub fn jump_distance_offset(&self) -> usize {
        use OpCode::*;
        match self {
            JUMP => 1,
            JUMP_IF => 2,
            JUMP_IF_EQ_I32 => 6,
            JUMP_IF_EQ_I64 => 2,
            JUMP_IF_EQ_BOOL => 3,
            JUMP_IF_EQ_STR => 5,
            JUMP_IF_EQ_CHAR => 5,
            _ => panic!("Only jumps have jump distance offset"),
        }
    }

    pub fn callee_addr_offset(&self) -> usize {
        use OpCode::*;
        match self {
            TAIL_CALL1 | TAIL_CALL2 | TAIL_CALL3 | TAIL_CALL_MANY => 1,
            CALL1 | CALL2 | CALL_MANY => 2,
            FUNCTION => 2,
            _ => panic!("Only calls have callee addrress offset"),
        }
    }
}

pub struct Chunk {
    code: Vec<u8>,
}

pub struct ChunkWriter<'c> {
    chunk: &'c mut Chunk,
    cnt: usize,
}

pub trait ToBytes<const N: usize> {
    fn to_bytes(self) -> [u8; N];
}

pub trait FromBytes<const N: usize> {
    fn from_bytes(bytes: [u8; N]) -> Self;
}

impl ToBytes<8> for i64 {
    fn to_bytes(self) -> [u8; 8] {
        self.to_le_bytes()
    }
}

impl FromBytes<8> for i64 {
    fn from_bytes(bytes: [u8; 8]) -> Self {
        Self::from_le_bytes(bytes)
    }
}

impl ToBytes<8> for u64 {
    fn to_bytes(self) -> [u8; 8] {
        self.to_le_bytes()
    }
}

impl FromBytes<8> for u64 {
    fn from_bytes(bytes: [u8; 8]) -> Self {
        Self::from_le_bytes(bytes)
    }
}

impl ToBytes<8> for f64 {
    fn to_bytes(self) -> [u8; 8] {
        self.to_le_bytes()
    }
}

impl FromBytes<8> for f64 {
    fn from_bytes(bytes: [u8; 8]) -> Self {
        Self::from_le_bytes(bytes)
    }
}

impl ToBytes<4> for StrAddr {
    fn to_bytes(self) -> [u8; 4] {
        self.0.to_le_bytes()
    }
}

impl FromBytes<4> for StrAddr {
    fn from_bytes(bytes: [u8; 4]) -> Self {
        Self(u32::from_le_bytes(bytes))
    }
}

impl ToBytes<4> for Addr {
    fn to_bytes(self) -> [u8; 4] {
        self.0.to_le_bytes()
    }
}

impl FromBytes<4> for Addr {
    fn from_bytes(bytes: [u8; 4]) -> Self {
        Self(u32::from_le_bytes(bytes))
    }
}

impl ToBytes<2> for BuiltinId {
    fn to_bytes(self) -> [u8; 2] {
        self.0.to_le_bytes()
    }
}

impl FromBytes<2> for BuiltinId {
    fn from_bytes(bytes: [u8; 2]) -> Self {
        Self(u16::from_le_bytes(bytes))
    }
}

impl ToBytes<2> for JumpDistance {
    fn to_bytes(self) -> [u8; 2] {
        self.0.to_le_bytes()
    }
}

impl FromBytes<2> for JumpDistance {
    fn from_bytes(bytes: [u8; 2]) -> Self {
        Self(i16::from_le_bytes(bytes))
    }
}

impl ToBytes<1> for OpCode {
    fn to_bytes(self) -> [u8; 1] {
        [self.into()]
    }
}

impl FromBytes<1> for OpCode {
    fn from_bytes(bytes: [u8; 1]) -> Self {
        Self::from_primitive(bytes[0])
    }
}

impl ToBytes<1> for bool {
    fn to_bytes(self) -> [u8; 1] {
        [if self { 1 } else { 0 }]
    }
}

impl FromBytes<1> for bool {
    fn from_bytes(bytes: [u8; 1]) -> Self {
        bytes[0] != 0
    }
}

impl ToBytes<4> for i32 {
    fn to_bytes(self) -> [u8; 4] {
        self.to_le_bytes()
    }
}

impl FromBytes<4> for i32 {
    fn from_bytes(bytes: [u8; 4]) -> Self {
        Self::from_le_bytes(bytes)
    }
}

impl ToBytes<4> for u32 {
    fn to_bytes(self) -> [u8; 4] {
        self.to_le_bytes()
    }
}

impl FromBytes<4> for u32 {
    fn from_bytes(bytes: [u8; 4]) -> Self {
        Self::from_le_bytes(bytes)
    }
}

impl ToBytes<1> for RegisterId {
    fn to_bytes(self) -> [u8; 1] {
        [self.0]
    }
}

impl FromBytes<1> for RegisterId {
    fn from_bytes(bytes: [u8; 1]) -> Self {
        Self(bytes[0])
    }
}

impl<const N: usize> ToBytes<N> for [RegisterId; N] {
    fn to_bytes(self) -> [u8; N] {
        self.map(|x| x.0)
    }
}

impl<const N: usize> FromBytes<N> for [RegisterId; N] {
    fn from_bytes(bytes: [u8; N]) -> Self {
        bytes.map(|x| RegisterId(x))
    }
}

impl ToBytes<1> for u8 {
    fn to_bytes(self) -> [u8; 1] {
        [self]
    }
}

impl FromBytes<1> for u8 {
    fn from_bytes(bytes: [u8; 1]) -> Self {
        bytes[0]
    }
}

impl ToBytes<4> for char {
    fn to_bytes(self) -> [u8; 4] {
        unsafe { std::mem::transmute(self) }
    }
}

impl FromBytes<4> for char {
    fn from_bytes(bytes: [u8; 4]) -> Self {
        unsafe { std::mem::transmute(bytes) }
    }
}

impl<'c> ChunkWriter<'c> {
    pub fn bytes<const N: usize>(mut self, code: [u8; N]) -> Self {
        self.chunk.code.extend(code.into_iter());
        self.cnt += N;
        self
    }

    pub fn push<T, const N: usize>(self, x: T) -> Self
    where
        T: ToBytes<N>,
    {
        self.bytes(x.to_bytes())
    }

    pub fn current(&self) -> usize {
        self.chunk.code.len()
    }

    pub fn finish(self) {
        let pad = self.cnt.div_ceil(8) * 8 - self.cnt;

        for _ in 0..pad {
            self.chunk.code.push(0);
        }
    }
}

pub struct ChunkReader<'c, const M: usize> {
    code: &'c [u8],
}

fn collect_to_array<T, const N: usize>(mut it: impl Iterator<Item = T>) -> [T; N]
where
    T: Default + Copy,
{
    let mut r = [T::default(); N];
    for i in 0..N {
        r[i] = it.next().unwrap();
    }
    r
}

impl<'c, const M: usize> ChunkReader<'c, M> {
    pub fn read<T, const N: usize>(self) -> (ChunkReader<'c, { M + N }>, T)
    where
        T: FromBytes<N>,
    {
        let r = T::from_bytes(collect_to_array(self.code[M..].iter().copied()));
        (ChunkReader { code: self.code }, r)
    }
}

impl Chunk {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn writer(&mut self) -> ChunkWriter {
        ChunkWriter {
            chunk: self,
            cnt: 0,
        }
    }

    pub fn write<T, const N: usize>(&mut self, addr: usize, value: T)
    where
        T: ToBytes<N>,
    {
        self.code[addr..addr + N]
            .iter_mut()
            .zip(value.to_bytes().into_iter())
            .for_each(|(write, value)| *write = value)
    }

    pub fn read<T, const N: usize>(&self, addr: Addr) -> T
    where
        T: FromBytes<N>,
    {
        let r = T::from_bytes(collect_to_array(
            self.code[usize::from(addr)..].iter().copied(),
        ));
        r
    }

    pub fn reader(&self, addr: Addr) -> ChunkReader<0> {
        ChunkReader {
            code: &self.code[addr.0 as usize..],
        }
    }

    pub fn fetch_registers<'a>(
        &'a self,
        at: Addr,
        cnt: usize,
    ) -> impl Iterator<Item = RegisterId> + 'a {
        (0..cnt).map(move |i| self.read::<RegisterId, _>(at + i as u32))
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn fill_back_jump(&mut self, addr: usize, distance: u16) {
        let [h, l] = distance.to_le_bytes();
        self.code[addr] = h;
        self.code[addr + 1] = l;
    }
}

impl std::ops::Index<usize> for Chunk {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        &self.code[index]
    }
}

impl std::ops::IndexMut<usize> for Chunk {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.code[index]
    }
}
