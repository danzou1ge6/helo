use std::collections::HashMap;

use crate::errors;
use crate::ir;
use crate::lir;

use helo_parse::errors::ManyError;
use helo_runtime::byte_code;

use byte_code::Instruction;
use helo_runtime::byte_code::JumpDistance;
use helo_runtime::executable;

pub struct FunctionTable {
    functions: HashMap<lir::FunctionId, byte_code::Addr>,
    names: Vec<(byte_code::Addr, byte_code::StrAddr)>,
}

impl FunctionTable {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            names: Vec::new(),
        }
    }
    pub fn get(&self, fid: lir::FunctionId) -> Option<byte_code::Addr> {
        self.functions.get(&fid).copied()
    }
    pub fn insert(
        &mut self,
        fid: lir::FunctionId,
        addr: byte_code::Addr,
        name: byte_code::StrAddr,
    ) {
        self.functions.insert(fid, addr);
        self.names.push((addr, name));
    }

    pub fn to_symbols(self) -> executable::Symbols {
        executable::Symbols::new(self.names)
    }
}

pub struct FunctionRelocations {
    relocations: Vec<(usize, lir::FunctionId)>,
}

impl FunctionRelocations {
    pub fn new() -> Self {
        Self {
            relocations: Vec::new(),
        }
    }
    pub fn add(&mut self, addr: usize, fid: lir::FunctionId) {
        self.relocations.push((addr, fid))
    }

    pub fn relocate(&self, chunk: &mut byte_code::Chunk, functions: &FunctionTable) {
        for (addr, fid) in self.relocations.iter() {
            let f_addr = functions.get(*fid).unwrap();
            chunk.write(*addr, f_addr);
        }
    }
}

struct ChunkRelocations {
    relocations: Vec<(usize, usize, lir::BlockId)>,
}

impl ChunkRelocations {
    pub fn new() -> Self {
        Self {
            relocations: Vec::new(),
        }
    }
    pub fn add(&mut self, addr: usize, origin: usize, bid: lir::BlockId) {
        self.relocations.push((addr, origin, bid))
    }
    pub fn relocate(
        &self,
        chunk: &mut byte_code::Chunk,
        block_addrs: &HashMap<lir::BlockId, usize>,
    ) -> Result<(), usize> {
        for (addr, origin, block_id) in self.relocations.iter() {
            let block_addr = block_addrs[block_id];
            let delta = if block_addr > *origin {
                block_addr - *origin
            } else {
                *origin - block_addr
            };
            if let Ok(delta) = byte_code::JumpDistance::try_from(delta) {
                chunk.write(*addr, delta);
            } else {
                return Err(delta);
            }
        }
        Ok(())
    }
}

pub fn lower_function(
    fid: lir::FunctionId,
    lir_functions: &lir::FunctionList<lir::FunctionOptimized>,
    str_index: &lir::StrIndex,
    chunk: &mut byte_code::Chunk,
    f_relocations: &mut FunctionRelocations,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) -> byte_code::Addr {
    let f = lir_functions.get(fid).unwrap();
    let addr = chunk.len();
    let addr = byte_code::Addr::from(addr as u32);
    functions.insert(fid, addr, str_index[f.name]);

    // Store arity and temp_cnt at function address
    chunk.writer().push(f.arity as u32);
    chunk.writer().push(f.temp_cnt as u32);

    let mut chunk_relocations = ChunkRelocations::new();
    let block_addrs = f
        .blocks
        .iter_id()
        .filter(|b| f.block_run[*b])
        .map(|block_id| {
            let addr = lower_block(
                block_id,
                &f.blocks,
                str_index,
                fid,
                &mut chunk_relocations,
                chunk,
                f_relocations,
            );
            (block_id, addr)
        })
        .collect::<HashMap<_, _>>();

    if let Err(distance) = chunk_relocations.relocate(chunk, &block_addrs) {
        e.push(errors::TooLongJump::new(&f.meta, distance))
    }

    if chunk.len() > u32::MAX as usize {
        e.push(errors::TooLongCode {
            current_len: chunk.len(),
        });
    }
    addr
}

fn lower_block(
    block_id: lir::BlockId,
    blocks: &lir::BlockHeap,
    str_index: &lir::StrIndex,
    current_fid: lir::FunctionId,
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
    f_relocations: &mut FunctionRelocations,
) -> usize {
    let addr = chunk.len();
    for (inst_index, inst) in blocks[block_id].iter().enumerate() {
        lower_instruction(
            inst_index,
            inst,
            str_index,
            block_id,
            blocks,
            current_fid,
            chunk,
            f_relocations,
        );
    }

    use lir::Jump::*;
    match blocks[block_id].exit() {
        JumpTable(test, table) => lower_jump_table(*test, &table, chunk_relocations, chunk),
        JumpIfElse(test, then, else_) => {
            lower_jump_if_else(*test, *then, *else_, chunk_relocations, chunk)
        }
        JumpSwitchInt(test, arms, default) => {
            lower_jump_switch_int(*test, &arms, *default, chunk_relocations, chunk)
        }
        JumpSwitchStr(test, arms, default) => {
            lower_jump_switch_str(*test, &arms, *default, str_index, chunk_relocations, chunk)
        }
        JumpSwitchChar(test, arms, default) => {
            lower_jump_switch_char(*test, &arms, *default, chunk_relocations, chunk)
        }
        Jump(block) => lower_jump(*block, chunk_relocations, chunk),
        Ret(r) => lower_ret(*r, chunk),
        Panic { file, span, msg } => lower_panic(*file, *msg, *span, str_index, chunk),
    };
    addr
}

fn lower_instruction(
    inst_index: usize,
    inst: &lir::Instruction,
    str_index: &lir::StrIndex,
    block: lir::BlockId,
    blocks: &lir::BlockHeap,
    current_fid: lir::FunctionId,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionRelocations,
) {
    use lir::Instruction::*;
    match inst {
        ApplyImpure(ret, callee, args) | Apply(ret, callee, args) => {
            lower_apply(*ret, *callee, &args, chunk)
        }
        CallBuiltin(ret, callee, args) | CallBuiltinImpure(ret, callee, args) => {
            lower_call_builtin(*ret, *callee, &args, chunk)
        }
        Call(ret, callee, args) | CallImpure(ret, callee, args) => {
            if is_return_addr(inst_index, inst, block, blocks) && *callee == current_fid {
                lower_tail_call(*callee, args, chunk, functions)
            } else {
                lower_call(*ret, *callee, &args, chunk, functions)
            }
        }
        CallThisClosure(ret, callee, args) | CallThisClosureImpure(ret, callee, args) => {
            if is_return_addr(inst_index, inst, block, blocks) {
                lower_tail_call_local(*callee, args, chunk)
            } else {
                lower_apply(*ret, *callee, &args, chunk)
            }
        }
        Int(to, v) => lower_int(*to, *v, chunk),
        Float(to, v) => lower_float(*to, v.parse().unwrap(), chunk),
        Bool(to, v) => lower_bool(*to, *v, chunk),
        Str(to, v) => lower_str(*to, *v, str_index, chunk),
        Char(to, v) => lower_char(*to, *v, chunk),
        Push(to, from, args) => lower_push(*to, *from, &args, chunk),
        AddToEnv(to, from, args) => lower_add_to_env(*to, *from, &args, chunk, functions),
        Function(to, fid) => lower_function_inst(*to, *fid, chunk, functions),
        Buitltin(to, bid) => lower_builtin(*to, *bid, chunk),
        Field(to, from, n) => lower_field(*to, *from, *n, chunk),
        Tagged(to, tag, args) => lower_tagged(*to, *tag, &args, chunk),
        Mov(to, from) => lower_mov(*to, *from, chunk),
    }
}

fn is_return_addr(
    inst_index: usize,
    inst: &lir::Instruction,
    block: lir::BlockId,
    blocks: &lir::BlockHeap,
) -> bool {
    inst_index + 1 == blocks[block].len()
        && (matches!(blocks[block].exit(), lir::Jump::Ret(Some(u)) if *u == inst.def())
            || matches!(blocks[block].exit(), lir::Jump::Ret(None)))
}

fn lower_jump_table(
    test: lir::TempId,
    table: &[lir::BlockId],
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
) {
    let inst_addr = chunk.len();
    Instruction::JumpTable(test.register(), table.len() as u8).emit(chunk);

    table
        .iter()
        .fold(chunk.writer(), |writer, b| {
            chunk_relocations.add(writer.current(), inst_addr, *b);
            writer.push::<byte_code::JumpDistance, _>(JumpDistance::default())
        })
        .finish();
}

fn lower_jump_if_else(
    test: lir::TempId,
    then_branch: lir::BlockId,
    else_branch: lir::BlockId,
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
) {
    let inst_addr = chunk.len();
    Instruction::JumpIf(test.register(), JumpDistance::default()).emit(chunk);
    let fill_back_addr = byte_code::OpCode::JUMP_IF.jump_distance_offset() + inst_addr;

    chunk_relocations.add(fill_back_addr, inst_addr, then_branch);

    let inst_addr = chunk.len();
    Instruction::Jump(JumpDistance::default()).emit(chunk);
    let fill_back_addr = byte_code::OpCode::JUMP.jump_distance_offset() + inst_addr;

    chunk_relocations.add(fill_back_addr, inst_addr, else_branch);
}

fn lower_jump_switch_int(
    test: lir::TempId,
    arms: &[(i64, lir::BlockId)],
    default: lir::BlockId,
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
) {
    for (value, branch) in arms {
        lower_jump_if_eq_int(test, *value, *branch, chunk_relocations, chunk);
    }
    lower_jump(default, chunk_relocations, chunk);
}

fn lower_jump_switch_str(
    test: lir::TempId,
    arms: &[(ir::StrId, lir::BlockId)],
    default: lir::BlockId,
    str_index: &lir::StrIndex,
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
) {
    for (value, branch) in arms {
        lower_jump_if_eq_str(test, *value, *branch, str_index, chunk_relocations, chunk);
    }
    lower_jump(default, chunk_relocations, chunk);
}

fn lower_jump_switch_char(
    test: lir::TempId,
    arms: &[(char, lir::BlockId)],
    default: lir::BlockId,
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
) {
    for (value, branch) in arms {
        lower_jump_if_eq_char(test, *value, *branch, chunk_relocations, chunk);
    }
    lower_jump(default, chunk_relocations, chunk);
}

fn lower_jump_if_eq_int(
    test: lir::TempId,
    value: i64,
    branch: lir::BlockId,
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
) {
    let inst_addr = chunk.len();
    let fill_back_addr = if i32::MIN as i64 <= value && value <= i32::MAX as i64 {
        Instruction::JumpIfEqI32(test.register(), value as i32, JumpDistance::default())
            .emit(chunk);
        inst_addr + byte_code::OpCode::JUMP_IF_EQ_I32.jump_distance_offset()
    } else {
        Instruction::JumpIfEqI64(test.register(), JumpDistance::default()).emit(chunk);
        chunk.writer().push(value);
        inst_addr + byte_code::OpCode::JUMP_IF_EQ_I64.jump_distance_offset()
    };
    chunk_relocations.add(fill_back_addr, inst_addr, branch);
}

fn lower_jump_if_eq_char(
    test: lir::TempId,
    value: char,
    branch: lir::BlockId,
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
) {
    let inst_addr = chunk.len();
    Instruction::JumpIfEqChar(test.register(), value, JumpDistance::default()).emit(chunk);
    let fill_back_addr = byte_code::OpCode::JUMP_IF_EQ_CHAR.jump_distance_offset() + inst_addr;

    chunk_relocations.add(fill_back_addr, inst_addr, branch);
}

fn lower_jump_if_eq_str(
    test: lir::TempId,
    value: ir::StrId,
    branch: lir::BlockId,
    str_index: &lir::StrIndex,
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
) {
    let inst_addr = chunk.len();
    Instruction::JumpIfEqStr(test.register(), str_index[value], JumpDistance::default())
        .emit(chunk);
    let fill_back_addr = byte_code::OpCode::JUMP_IF_EQ_STR.jump_distance_offset() + inst_addr;

    chunk_relocations.add(fill_back_addr, inst_addr, branch);
}

fn lower_jump(
    branch: lir::BlockId,
    chunk_relocations: &mut ChunkRelocations,
    chunk: &mut byte_code::Chunk,
) {
    let inst_addr = chunk.len();
    Instruction::Jump(JumpDistance::default()).emit(chunk);
    let fill_back_addr = byte_code::OpCode::JUMP.jump_distance_offset() + inst_addr;

    chunk_relocations.add(fill_back_addr, inst_addr, branch);
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

fn lower_apply(
    ret: lir::TempId,
    callee: lir::TempId,
    args: &[lir::TempId],
    chunk: &mut byte_code::Chunk,
) {
    let ret = ret.register();
    let callee = callee.register();
    let args_len = args.len();
    let args = args.iter().map(|a| a.register());

    if args_len <= 5 {
        let inst = match args_len {
            1 => Instruction::Apply1(ret, callee, collect_to_array(args)),
            2 => Instruction::Apply2(ret, callee, collect_to_array(args)),
            3 => Instruction::Apply3(ret, callee, collect_to_array(args)),
            4 => Instruction::Apply4(ret, callee, collect_to_array(args)),
            5 => Instruction::Apply5(ret, callee, collect_to_array(args)),
            _ => unreachable!(),
        };
        inst.emit(chunk)
    } else {
        Instruction::ApplyMany(ret, callee, args_len as u8).emit(chunk);
        args.fold(chunk.writer(), |writer, arg| writer.push(arg))
            .finish();
    }
}

fn lower_tail_call_local(callee: lir::TempId, args: &[lir::TempId], chunk: &mut byte_code::Chunk) {
    let callee = callee.register();
    let args_len = args.len();
    let args = args.iter().map(|a| a.register());

    if args_len <= 6 {
        let inst = match args_len {
            1 => Instruction::TailCallLocal1(callee, collect_to_array(args)),
            2 => Instruction::TailCallLocal2(callee, collect_to_array(args)),
            3 => Instruction::TailCallLocal3(callee, collect_to_array(args)),
            4 => Instruction::TailCallLocal4(callee, collect_to_array(args)),
            5 => Instruction::TailCallLocal5(callee, collect_to_array(args)),
            6 => Instruction::TailCallLocal6(callee, collect_to_array(args)),
            _ => unreachable!(),
        };
        inst.emit(chunk)
    } else {
        Instruction::TailCallLocalMany(callee, args_len as u8).emit(chunk);
        args.fold(chunk.writer(), |writer, arg| writer.push(arg))
            .finish();
    }
}

fn lower_tail_call(
    callee: lir::FunctionId,
    args: &[lir::TempId],
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionRelocations,
) {
    let callee_addr = byte_code::Addr::default();
    let args_len = args.len();
    let args = args.iter().map(|a| a.register());

    functions.add(
        chunk.len() + byte_code::OpCode::TAIL_CALL1.callee_addr_offset(),
        callee,
    );

    if args_len <= 3 {
        let inst = match args_len {
            1 => Instruction::TailCall1(callee_addr, collect_to_array(args)),
            2 => Instruction::TailCall2(callee_addr, collect_to_array(args)),
            3 => Instruction::TailCall3(callee_addr, collect_to_array(args)),
            _ => unreachable!(),
        };
        inst.emit(chunk)
    } else {
        Instruction::TailCallMany(callee_addr, args_len as u8).emit(chunk);
        args.fold(chunk.writer(), |writer, arg| writer.push(arg))
            .finish();
    }
}

fn lower_call_builtin(
    ret: lir::TempId,
    callee: lir::BuiltinId,
    args: &[lir::TempId],
    chunk: &mut byte_code::Chunk,
) {
    let ret = ret.register();
    let args_len = args.len();
    let args = args.iter().map(|a| a.register());

    let inst = match args_len {
        1 => Instruction::CallBuiltin1(ret, callee, collect_to_array(args)),
        2 => Instruction::CallBuiltin2(ret, callee, collect_to_array(args)),
        3 => Instruction::CallBuiltin3(ret, callee, collect_to_array(args)),
        4 => Instruction::CallBuiltin4(ret, callee, collect_to_array(args)),
        _ => panic!("we shouldnt have builtin with more than 4 arguments, or do we?"),
    };
    inst.emit(chunk)
}

fn lower_call(
    ret: lir::TempId,
    callee: lir::FunctionId,
    args: &[lir::TempId],
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionRelocations,
) {
    let ret = ret.register();
    let callee_addr = byte_code::Addr::default();
    let args_len = args.len();
    let args = args.iter().map(|a| a.register());

    functions.add(
        chunk.len() + byte_code::OpCode::CALL1.callee_addr_offset(),
        callee,
    );

    if args_len <= 2 {
        let inst = match args_len {
            1 => Instruction::Call1(ret, callee_addr, collect_to_array(args)),
            2 => Instruction::Call2(ret, callee_addr, collect_to_array(args)),
            _ => unreachable!(),
        };
        inst.emit(chunk)
    } else {
        Instruction::CallMany(ret, callee_addr, args_len as u8).emit(chunk);
        args.fold(chunk.writer(), |writer, arg| writer.push(arg))
            .finish();
    }
}

fn lower_int(to: lir::TempId, value: i64, chunk: &mut byte_code::Chunk) {
    let to = to.register();
    if i32::MIN as i64 <= value && value <= i32::MAX as i64 {
        Instruction::Int32(to, value as i32).emit(chunk);
    } else {
        Instruction::Int64(to).emit(chunk);
        chunk.writer().push(value).finish();
    };
}

fn lower_float(to: lir::TempId, value: f64, chunk: &mut byte_code::Chunk) {
    Instruction::Float(to.register()).emit(chunk);
    chunk.writer().push(value).finish();
}

fn lower_char(to: lir::TempId, value: char, chunk: &mut byte_code::Chunk) {
    Instruction::Char(to.register(), value).emit(chunk);
}

fn lower_bool(to: lir::TempId, value: bool, chunk: &mut byte_code::Chunk) {
    Instruction::Bool(to.register(), value).emit(chunk);
}

fn lower_str(
    to: lir::TempId,
    value: ir::StrId,
    str_index: &lir::StrIndex,
    chunk: &mut byte_code::Chunk,
) {
    Instruction::Str(to.register(), str_index[value]).emit(chunk);
}

fn lower_add_to_env(
    to: lir::TempId,
    from: lir::TempId,
    mut args: &[lir::TempId],
    chunk: &mut byte_code::Chunk,
    _functions: &mut FunctionRelocations,
) {
    let to = to.register();
    let from = from.register();

    while args.len() > 6 {
        Instruction::AddToEnv6(
            from,
            collect_to_array(args[0..6].iter().map(|a| a.register())),
        )
        .emit(chunk);
        args = &args[6..]
    }

    let args = args.iter().map(|a| a.register());
    let inst = match args.len() {
        0 => return,
        1 => Instruction::AddToEnv1(from, collect_to_array(args)),
        2 => Instruction::AddToEnv2(from, collect_to_array(args)),
        3 => Instruction::AddToEnv3(from, collect_to_array(args)),
        4 => Instruction::AddToEnv4(from, collect_to_array(args)),
        5 => Instruction::AddToEnv5(from, collect_to_array(args)),
        6 => Instruction::AddToEnv6(from, collect_to_array(args)),
        _ => unreachable!(),
    };

    inst.emit(chunk);
    Instruction::Mov(from, to).emit(chunk);
}

fn lower_push(
    to: lir::TempId,
    from: lir::TempId,
    mut args: &[lir::TempId],
    chunk: &mut byte_code::Chunk,
) {
    let to = to.register();
    let from = from.register();
    if to != from {
        Instruction::Mov(to, from).emit(chunk);
    }

    while args.len() > 6 {
        Instruction::Push6(
            to,
            collect_to_array(args[0..6].iter().map(|a| a.register())),
        )
        .emit(chunk);
        args = &args[6..];
    }

    let args = args.iter().map(|a| a.register());

    let inst = match args.len() {
        0 => return,
        1 => Instruction::Push1(to, collect_to_array(args)),
        2 => Instruction::Push2(to, collect_to_array(args)),
        3 => Instruction::Push3(to, collect_to_array(args)),
        4 => Instruction::Push4(to, collect_to_array(args)),
        5 => Instruction::Push5(to, collect_to_array(args)),
        6 => Instruction::Push6(to, collect_to_array(args)),
        _ => unreachable!(),
    };

    inst.emit(chunk)
}

fn lower_function_inst(
    to: lir::TempId,
    fid: lir::FunctionId,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionRelocations,
) {
    functions.add(
        chunk.len() + byte_code::OpCode::FUNCTION.callee_addr_offset(),
        fid,
    );
    Instruction::Function(to.register(), byte_code::Addr::default()).emit(chunk);
}

fn lower_builtin(to: lir::TempId, bid: lir::BuiltinId, chunk: &mut byte_code::Chunk) {
    Instruction::Builtin(to.register(), bid).emit(chunk);
}

fn lower_field(to: lir::TempId, from: lir::TempId, n: usize, chunk: &mut byte_code::Chunk) {
    Instruction::Field(to.register(), from.register(), n as u8).emit(chunk);
}

fn lower_panic(
    file: ir::StrId,
    msg: ir::StrId,
    (span0, span1): (usize, usize),
    str_index: &lir::StrIndex,
    chunk: &mut byte_code::Chunk,
) {
    Instruction::Panic(str_index[msg]).emit(chunk);
    chunk
        .writer()
        .push(span0 as u32)
        .push(span1 as u32)
        .push(str_index[file])
        .finish();
}

fn lower_tagged(to: lir::TempId, tag: u8, args: &[lir::TempId], chunk: &mut byte_code::Chunk) {
    let to = to.register();

    if args.len() == 0 {
        Instruction::Tagged(to, tag).emit(chunk);
        return;
    }

    let args = args.iter().map(|a| a.register());
    if args.len() <= 5 {
        let inst = match args.len() {
            1 => Instruction::Tagged1(to, tag, collect_to_array(args)),
            2 => Instruction::Tagged2(to, tag, collect_to_array(args)),
            3 => Instruction::Tagged3(to, tag, collect_to_array(args)),
            4 => Instruction::Tagged4(to, tag, collect_to_array(args)),
            5 => Instruction::Tagged5(to, tag, collect_to_array(args)),
            _ => unreachable!(),
        };
        inst.emit(chunk);
        return;
    }
}

fn lower_mov(to: lir::TempId, from: lir::TempId, chunk: &mut byte_code::Chunk) {
    Instruction::Mov(to.register(), from.register()).emit(chunk);
}

fn lower_ret(from: Option<lir::TempId>, chunk: &mut byte_code::Chunk) {
    if let Some(from) = from {
        Instruction::Ret(from.register()).emit(chunk)
    } else {
        Instruction::RetNone.emit(chunk)
    }
}
