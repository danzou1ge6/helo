use num_enum::{FromPrimitive, IntoPrimitive};

/// Takes 1 bytes
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct RegisterId(pub(crate) u8);
/// Takes 3 bytes
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct StrId(pub(crate) u32);
/// Take 2 bytes
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct BuiltinId(pub(crate) u16);
/// Take 4 bytes
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct FunctionAddr(pub(crate) u32);

impl From<u8> for RegisterId {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl From<u32> for StrId {
    fn from(value: u32) -> Self {
        if value > 2_u32.pow(24) - 1 {
            panic!("StrId must be within 3 bytes")
        }
        Self(value)
    }
}

impl From<u32> for FunctionAddr {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<u16> for BuiltinId {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

pub enum Instruction {
    /// Following is a table of relative jump distance. The `i`th entry is taken for tag `i` at .0.
    /// The relative jump distance is the difference of address of jump target and address of this instruction.
    /// Relative jump distance takes one byte
    ///
    /// 2 of 8 bytes
    JumpTable(RegisterId),
    /// Perform a relative jump if .0 is true.
    ///
    /// 4 of 8 bytes
    JumpIf(RegisterId, u16),
    /// Jumps if .0 equals .1. .1 takes 4 bytes
    ///
    /// 8 of 8 bytes
    JumpIfEqI32(RegisterId, i32, u16),
    /// Jump if .0 equals i64 stored in next 8 bytes.
    ///
    /// 4 of 8 bytes
    JumpIfEqI64(RegisterId, u16, i64),
    /// Jump if .0 equals string at .1.
    ///
    /// 7 of 8 bytes
    JumpIfEqStr(RegisterId, StrId, u16),
    /// Jump if .0 equals bool at .1
    ///
    /// 5 of 8 bytes
    JumpIfEqBool(RegisterId, bool, u16),
    /// Jump a relative distance
    ///
    /// 3 of 8 bytes
    Jump(u16),

    /// Apply .2 to .1 and store result to .0
    ///
    /// 4 of 8 bytes
    Apply1(RegisterId, RegisterId, Vec<RegisterId>),
    /// 5 of 8 bytes
    Apply2(RegisterId, RegisterId, Vec<RegisterId>),
    /// 6 of 8 bytes
    Apply3(RegisterId, RegisterId, Vec<RegisterId>),
    /// 7 of 8 bytes
    Apply4(RegisterId, RegisterId, Vec<RegisterId>),
    /// 8 of 8 bytes
    Apply5(RegisterId, RegisterId, Vec<RegisterId>),
    /// Apply the following .2 number of register-ids to .1
    ///
    /// 4 of 8 bytes
    ApplyMany(RegisterId, RegisterId, u8),

    /// Call .1 with .2 and store result to .0
    ///
    /// 7 of 8 bytes
    Call1(RegisterId, FunctionAddr, Vec<RegisterId>),
    /// 8 of 8 bytes
    Call2(RegisterId, FunctionAddr, Vec<RegisterId>),
    /// Call .1 with following .2 number of registers
    ///
    /// 4 of 8 bytes
    CallMany(RegisterId, FunctionAddr, u8),

    /// Tail call .1 with .2 and store result to .0
    ///
    /// 7 of 8 bytes
    TailCallU1(RegisterId, FunctionAddr, Vec<RegisterId>),
    /// 8 of 8 bytes
    TailCallU2(RegisterId, FunctionAddr, Vec<RegisterId>),
    /// Tail call .1 with following .2 number of registers
    ///
    /// 4 of 8 bytes
    TailCallUMany(RegisterId, FunctionAddr, u8),

    /// Tail call .1 with .2 and store result to .0
    ///
    /// 4 of 8 bytes
    TailCall1(RegisterId, RegisterId, Vec<RegisterId>),
    /// 5 of 8 bytes
    TailCall2(RegisterId, RegisterId, Vec<RegisterId>),
    /// 6 of 8 bytes
    TailCall3(RegisterId, RegisterId, Vec<RegisterId>),
    /// 7 of 8 bytes
    TailCall4(RegisterId, RegisterId, Vec<RegisterId>),
    /// 8 of 8 bytes
    TailCall5(RegisterId, RegisterId, Vec<RegisterId>),
    /// Call .1 with following .2 number of registers
    ///
    /// 4 of 8 bytes
    TailCallMany(RegisterId, RegisterId, u8),

    /// call builtin .1 with .2 and store result to .0
    ///
    /// 5 of 8 bytes
    CallBuiltin1(RegisterId, BuiltinId, Vec<RegisterId>),
    /// 6 of 8 bytes
    CallBuiltin2(RegisterId, BuiltinId, Vec<RegisterId>),
    /// 7 of 8 bytes
    CallBuiltin3(RegisterId, BuiltinId, Vec<RegisterId>),
    /// 8 of 8 bytes
    CallBuiltin4(RegisterId, BuiltinId, Vec<RegisterId>),

    /// Load immediate to register .0
    ///
    /// 6 of 8 bytes
    Int32(RegisterId, i32),
    /// 3 of 8 bytes
    Bool(RegisterId, bool),
    /// 5 of 8 bytes
    Str(RegisterId, StrId),
    /// Load Immediate at following 8-bytes to .0
    ///
    /// 2 of 8 bytes
    Int64(RegisterId, i64),
    /// 2 of 8 bytes
    Float(RegisterId, f64),

    /// Push .1 to list at .0. Variants, tuples and closures are both represented by lists.
    ///
    /// 3 of 8 bytes
    Push1(RegisterId, Vec<RegisterId>),
    /// 4 of 8 bytes
    Push2(RegisterId, Vec<RegisterId>),
    /// 5 of 8 bytes
    Push3(RegisterId, Vec<RegisterId>),
    /// 6 of 8 bytes
    Push4(RegisterId, Vec<RegisterId>),
    /// 7 of 8 bytes
    Push5(RegisterId, Vec<RegisterId>),
    /// 8 of 8 bytes
    Push6(RegisterId, Vec<RegisterId>),

    /// Load a user function
    ///
    /// 6 of 8 bytes
    Function(RegisterId, FunctionAddr),
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
    Tagged1(RegisterId, u8, Vec<RegisterId>),
    /// 5 of 8 bytes
    Tagged2(RegisterId, u8, Vec<RegisterId>),
    /// 6 of 8 bytes
    Tagged3(RegisterId, u8, Vec<RegisterId>),
    /// 7 of 8 bytes
    Tagged4(RegisterId, u8, Vec<RegisterId>),
    /// 8 of 8 bytes
    Tagged5(RegisterId, u8, Vec<RegisterId>),

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
    Panic(StrId),
}

#[derive(FromPrimitive, IntoPrimitive, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum OpCode {
    JUMP_TABLE,
    JUMP_IF,
    JUMP_IF_EQ_I32,
    JUMP_IF_EQ_I64,
    JUMP_IF_EQ_STR,
    JUMP_IF_EQ_BOOL,
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
    TAIL_CALL_U1,
    TAIL_CALL_U2,
    TAIL_CALL_U_MANY,
    TAIL_CALL1,
    TAIL_CALL2,
    TAIL_CALL3,
    TAIL_CALL4,
    TAIL_CALL5,
    TAIL_CALL_MANY,
    CALL_BUILTIN1,
    CALL_BUILTIN2,
    CALL_BUILTIN3,
    CALL_BUILTIN4,
    INT32,
    BOOL,
    STR,
    INT64,
    FLOAT,
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

impl OpCode {
    pub fn jump_distance_offset(&self) -> usize {
        use OpCode::*;
        match self {
            JUMP => 0,
            JUMP_IF => 2,
            JUMP_IF_EQ_I32 => 6,
            JUMP_IF_EQ_I64 => 2,
            JUMP_IF_EQ_BOOL => 3,
            JUMP_IF_EQ_STR => 5,
            _ => panic!("Only jumps have jump distance offset"),
        }
    }
}

impl Instruction {
    pub fn emit(self, chunk: &mut Chunk) {
        use Instruction::*;
        use OpCode::*;

        let writer = chunk.writer();

        match self {
            JumpTable(r) => writer.op_code(JUMP_TABLE).register(r).finish(),
            JumpIf(r, d) => writer.op_code(JUMP_IF).register(r).u16_(d).finish(),
            JumpIfEqBool(r, v, d) => writer
                .op_code(JUMP_IF_EQ_BOOL)
                .register(r)
                .bool_(v)
                .u16_(d)
                .finish(),
            JumpIfEqI32(r, v, d) => writer
                .op_code(JUMP_IF_EQ_I32)
                .register(r)
                .i32_(v)
                .u16_(d)
                .finish(),
            JumpIfEqI64(r, d, v) => {
                writer.op_code(JUMP_IF_EQ_I64).register(r).u16_(d).finish();
                chunk.writer().i64_(v).finish()
            }
            JumpIfEqStr(r, v, d) => writer
                .op_code(JUMP_IF_EQ_STR)
                .register(r)
                .str_id(v)
                .u16_(d)
                .finish(),
            Jump(d) => writer.op_code(JUMP).u16_(d).finish(),
            Apply1(re, c, arg)
            | Apply2(re, c, arg)
            | Apply3(re, c, arg)
            | Apply4(re, c, arg)
            | Apply5(re, c, arg) => writer
                .op_code(APPLY1)
                .register(re)
                .register(c)
                .registers(arg)
                .finish(),
            ApplyMany(re, c, cnt) => writer
                .op_code(APPLY_MANY)
                .register(re)
                .register(c)
                .byte(cnt)
                .finish(),
            Call1(re, c, arg) | Call2(re, c, arg) => writer
                .op_code(CALL1)
                .register(re)
                .function_id(c)
                .registers(arg)
                .finish(),
            CallMany(re, c, cnt) => writer
                .op_code(CALL_MANY)
                .register(re)
                .function_id(c)
                .byte(cnt)
                .finish(),
            TailCallU1(re, c, arg) | TailCallU2(re, c, arg) => writer
                .op_code(TAIL_CALL_U1)
                .register(re)
                .function_id(c)
                .registers(arg)
                .finish(),
            TailCallUMany(re, c, cnt) => writer
                .op_code(TAIL_CALL_U_MANY)
                .register(re)
                .function_id(c)
                .byte(cnt)
                .finish(),
            TailCall1(re, c, arg)
            | TailCall2(re, c, arg)
            | TailCall3(re, c, arg)
            | TailCall4(re, c, arg)
            | TailCall5(re, c, arg) => writer
                .op_code(TAIL_CALL1)
                .register(re)
                .register(c)
                .registers(arg)
                .finish(),
            TailCallMany(re, c, cnt) => writer
                .op_code(TAIL_CALL_MANY)
                .register(re)
                .register(c)
                .byte(cnt)
                .finish(),
            CallBuiltin1(re, c, arg)
            | CallBuiltin2(re, c, arg)
            | CallBuiltin3(re, c, arg)
            | CallBuiltin4(re, c, arg) => writer
                .op_code(CALL_BUILTIN1)
                .register(re)
                .builtin_id(c)
                .registers(arg)
                .finish(),
            Int32(r, v) => writer.op_code(INT32).register(r).i32_(v).finish(),
            Bool(r, v) => writer.op_code(BOOL).register(r).bool_(v).finish(),
            Str(r, v) => writer.op_code(STR).register(r).str_id(v).finish(),
            Int64(r, v) => {
                writer.op_code(INT64).register(r).finish();
                chunk.writer().i64_(v).finish();
            }
            Float(r, v) => {
                writer.op_code(FLOAT).register(r).finish();
                chunk.writer().f64_(v).finish();
            }
            Push1(r, arg)
            | Push2(r, arg)
            | Push3(r, arg)
            | Push4(r, arg)
            | Push5(r, arg)
            | Push6(r, arg) => writer.op_code(PUSH1).register(r).registers(arg).finish(),
            Function(r, f) => writer.op_code(FUNCTION).register(r).function_id(f).finish(),
            Builtin(r, f) => writer.op_code(BUILTIN).register(r).builtin_id(f).finish(),
            Field(re, o, n) => writer
                .op_code(FIELD)
                .register(re)
                .register(o)
                .byte(n)
                .finish(),
            Tagged1(r, t, arg)
            | Tagged2(r, t, arg)
            | Tagged3(r, t, arg)
            | Tagged4(r, t, arg)
            | Tagged5(r, t, arg) => writer
                .op_code(TAGGED1)
                .register(r)
                .byte(t)
                .registers(arg)
                .finish(),
            Tagged(r, t) => writer.op_code(TAGGED).register(r).byte(t).finish(),
            Mov(re, r) => writer.op_code(MOV).register(re).register(r).finish(),
            Ret(r) => writer.op_code(RET).register(r).finish(),
            Panic(s) => writer.op_code(PANIC).str_id(s).finish(),
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

fn u32_to_4u8(n: u32) -> [u8; 4] {
    let t = ((n >> 24) & 0xff) as u8;
    let h = ((n >> 16) & 0xff) as u8;
    let m = ((n >> 8) & 0xff) as u8;
    let l = (n & 0xff) as u8;
    [t, h, m, l]
}
fn i32_to_4u8(n: i32) -> [u8; 4] {
    let t = ((n >> 24) & 0xff) as u8;
    let h = ((n >> 16) & 0xff) as u8;
    let m = ((n >> 8) & 0xff) as u8;
    let l = (n & 0xff) as u8;
    [t, h, m, l]
}
fn i64_to_8u8(n: i64) -> [u8; 8] {
    let a = ((n >> 56) & 0xff) as u8;
    let b = ((n >> 48) & 0xff) as u8;
    let c = ((n >> 40) & 0xff) as u8;
    let d = ((n >> 32) & 0xff) as u8;
    let e = ((n >> 24) & 0xff) as u8;
    let f = ((n >> 16) & 0xff) as u8;
    let g = ((n >> 8) & 0xff) as u8;
    let h = (n & 0xff) as u8;
    [a, b, c, d, e, f, g, h]
}
fn f64_to_8u8(n: f64) -> [u8; 8] {
    unsafe { std::mem::transmute::<_, _>(n) }
}
fn u16_to_2u8(n: u16) -> [u8; 2] {
    let g = ((n >> 8) & 0xff) as u8;
    let h = (n & 0xff) as u8;
    [g, h]
}

impl<'c> ChunkWriter<'c> {
    pub fn i64_(mut self, n: i64) -> Self {
        self.cnt += 8;
        self.bytes(i64_to_8u8(n))
    }

    pub fn f64_(mut self, n: f64) -> Self {
        self.cnt += 8;
        self.bytes(f64_to_8u8(n))
    }

    pub fn str_id(mut self, str_id: StrId) -> Self {
        let n = str_id.0;
        let h = ((n >> 16) & 0xff) as u8;
        let m = ((n >> 8) & 0xff) as u8;
        let l = (n & 0xff) as u8;
        self.cnt += 3;
        self.bytes([h, m, l])
    }

    pub fn function_id(mut self, function_id: FunctionAddr) -> Self {
        self.cnt += 4;
        self.bytes(u32_to_4u8(function_id.0))
    }

    pub fn builtin_id(mut self, builtin_id: BuiltinId) -> Self {
        let n = builtin_id.0;
        self.cnt += 2;
        self.bytes(u16_to_2u8(n))
    }

    pub fn u16_(mut self, value: u16) -> Self {
        self.cnt += 2;
        self.bytes(u16_to_2u8(value))
    }

    pub fn byte(mut self, x: u8) -> Self {
        self.chunk.code.push(x);
        self.cnt += 1;
        self
    }

    pub fn op_code(mut self, op: OpCode) -> Self {
        self.chunk.code.push(op.into());
        self.cnt += 1;
        self
    }

    pub fn bool_(mut self, value: bool) -> Self {
        self.chunk.code.push(if value { 1 } else { 0 });
        self.cnt += 1;
        self
    }

    pub fn bytes<const N: usize>(mut self, code: [u8; N]) -> Self {
        self.chunk.code.extend(code.into_iter());
        self.cnt += code.len();
        self
    }

    pub fn i32_(mut self, value: i32) -> Self {
        self.cnt += 4;
        self.bytes(i32_to_4u8(value))
    }

    pub fn register(self, reg: RegisterId) -> Self {
        self.byte(reg.0)
    }

    pub fn registers(mut self, regs: Vec<RegisterId>) -> Self {
        self.cnt += regs.len();
        regs.into_iter().for_each(|r| self.chunk.code.push(r.0));
        self
    }

    pub fn finish(self) {
        let pad = self.cnt.div_ceil(8) * 8 - self.cnt;

        for _ in 0..pad {
            self.chunk.code.push(0);
        }
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

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn fill_back_jump(&mut self, addr: usize, distance: u16) {
        let [h, l] = u16_to_2u8(distance);
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
