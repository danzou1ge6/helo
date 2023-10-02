use std::collections::HashMap;

use crate::errors;
use crate::ir;
use crate::lir;

use helo_parse::ast::Meta;
use helo_parse::errors::ManyError;
use helo_runtime::byte_code;

use byte_code::Instruction;
use helo_runtime::executable;

pub struct FunctionTable {
    functions: HashMap<lir::FunctionId, byte_code::FunctionAddr>,
    names: Vec<(byte_code::FunctionAddr, byte_code::StrAddr)>,
}

impl FunctionTable {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            names: Vec::new(),
        }
    }
    pub fn get(&self, fid: lir::FunctionId) -> Option<byte_code::FunctionAddr> {
        self.functions.get(&fid).copied()
    }
    pub fn insert(
        &mut self,
        fid: lir::FunctionId,
        addr: byte_code::FunctionAddr,
        name: byte_code::StrAddr,
    ) {
        self.functions.insert(fid, addr);
        self.names.push((addr, name));
    }

    pub fn to_symbols(self) -> executable::Symbols {
        executable::Symbols::new(self.names)
    }
}

pub fn lower_function(
    fid: lir::FunctionId,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) -> byte_code::FunctionAddr {
    if let Some(addr) = functions.get(fid) {
        addr
    } else {
        let f = lir_functions.get(fid).unwrap();
        let addr = chunk.len();
        if addr > u32::MAX as usize {
            e.push(errors::TooLongCode { current_len: addr });
        }
        let addr = byte_code::FunctionAddr::from(addr as u32);
        functions.insert(fid, addr, str_index[f.name]);
        lower_block(
            f.body,
            &f.blocks,
            lir_functions,
            str_index,
            &f.meta,
            chunk,
            functions,
            e,
        );
        addr
    }
}

fn lower_block(
    block_id: lir::BlockId,
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) -> usize {
    let addr = chunk.len();
    for inst in blocks[block_id].iter() {
        lower_instruction(
            inst,
            blocks,
            lir_functions,
            str_index,
            function_meta,
            chunk,
            functions,
            e,
        );
    }
    addr
}

fn lower_instruction(
    inst: &lir::Instruction,
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    use lir::Instruction::*;
    match inst {
        JumpTable(t, table) => lower_jump_table(
            *t,
            &table,
            blocks,
            lir_functions,
            str_index,
            function_meta,
            chunk,
            functions,
            e,
        ),
        JumpIf(t, b) => lower_jump_if(
            *t,
            *b,
            blocks,
            lir_functions,
            str_index,
            function_meta,
            chunk,
            functions,
            e,
        ),
        JumpIfElse(t, then_branch, else_branch) => lower_jump_if_else(
            *t,
            *then_branch,
            *else_branch,
            blocks,
            lir_functions,
            str_index,
            function_meta,
            chunk,
            functions,
            e,
        ),
        JumpIfEqInt(t, v, b) => lower_jump_if_eq_int(
            *t,
            *v,
            *b,
            blocks,
            lir_functions,
            str_index,
            function_meta,
            chunk,
            functions,
            e,
        ),
        JumpIfEqBool(t, v, b) => lower_jump_if_eq_bool(
            *t,
            *v,
            *b,
            blocks,
            lir_functions,
            str_index,
            function_meta,
            chunk,
            functions,
            e,
        ),
        JumpIfEqStr(t, v, b) => lower_jump_if_eq_str(
            *t,
            *v,
            *b,
            blocks,
            lir_functions,
            str_index,
            function_meta,
            chunk,
            functions,
            e,
        ),
        Jump(b) => lower_jump(
            *b,
            blocks,
            lir_functions,
            str_index,
            function_meta,
            chunk,
            functions,
            e,
        ),
        Apply(ret, callee, args) => lower_apply(*ret, *callee, &args, chunk),
        TailCall(ret, callee, args) => lower_tail_call(*ret, *callee, &args, chunk),
        TailCallU(ret, callee, args) => lower_tail_call_u(
            *ret,
            *callee,
            &args,
            lir_functions,
            str_index,
            chunk,
            functions,
            e,
        ),
        CallBuiltin(ret, callee, args) => lower_call_builtin(*ret, *callee, &args, chunk),
        Call(ret, callee, args) => lower_call(
            *ret,
            *callee,
            &args,
            lir_functions,
            str_index,
            chunk,
            functions,
            e,
        ),
        Int(to, v) => lower_int(*to, *v, chunk),
        Float(to, v) => lower_float(*to, v.parse().unwrap(), chunk),
        Bool(to, v) => lower_bool(*to, *v, chunk),
        Str(to, v) => lower_str(*to, *v, str_index, chunk),
        Push(to, from, args) => lower_push(*to, *from, &args, chunk),
        Function(to, fid) => {
            lower_function_inst(*to, *fid, lir_functions, str_index, chunk, functions, e)
        }
        Buitltin(to, bid) => lower_builtin(*to, *bid, chunk),
        Field(to, from, n) => lower_field(*to, *from, *n, chunk),
        Panic(s) => lower_panic(*s, str_index, chunk),
        Tagged(to, tag, args) => lower_tagged(*to, *tag, &args, chunk),
        Mov(to, from) => lower_mov(*to, *from, chunk),
        Ret(r) => lower_ret(*r, chunk),
    }
}

fn fill_back_jump(
    delta: usize,
    addr: usize,
    chunk: &mut byte_code::Chunk,
    function_meta: &Meta,
    e: &mut ManyError,
) {
    if delta > u16::MAX as usize {
        e.push(errors::TooLongJump::new(function_meta, delta));
    }
    chunk.fill_back_jump(addr, delta as u16);
}

fn lower_branch_and_fill_back(
    inst: Instruction,
    fill_back_addr: usize,
    branch: lir::BlockId,
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    let inst_addr = chunk.len();
    inst.emit(chunk);
    let branch_addr = lower_block(
        branch,
        blocks,
        lir_functions,
        str_index,
        function_meta,
        chunk,
        functions,
        e,
    );
    fill_back_jump(
        branch_addr - inst_addr,
        fill_back_addr,
        chunk,
        function_meta,
        e,
    );
}

fn lower_jump_table(
    test: lir::TempId,
    table: &[lir::BlockId],
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    let inst = Instruction::JumpTable(test.register());
    let inst_addr = chunk.len();
    inst.emit(chunk);

    table
        .iter()
        .fold(chunk.writer(), |writer, _| writer.push(0))
        .finish();

    let branch_addrs: Vec<_> = table
        .iter()
        .map(|b| {
            lower_block(
                *b,
                blocks,
                lir_functions,
                str_index,
                function_meta,
                chunk,
                functions,
                e,
            )
        })
        .collect();
    branch_addrs
        .into_iter()
        .zip(inst_addr + 1..inst_addr + 1 + table.len())
        .for_each(|(branch_addr, entry_addr)| {
            // branch_addr must be bigger, as branches are inserted later
            let delta = branch_addr - inst_addr;
            fill_back_jump(delta, entry_addr, chunk, function_meta, e)
        });
}

fn lower_jump_if(
    test: lir::TempId,
    branch: lir::BlockId,
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    let inst = Instruction::JumpIf(test.register(), 0);
    let fill_back_addr = byte_code::OpCode::JUMP_IF.jump_distance_offset() + chunk.len();
    lower_branch_and_fill_back(
        inst,
        fill_back_addr,
        branch,
        blocks,
        lir_functions,
        str_index,
        function_meta,
        chunk,
        functions,
        e,
    );
}

fn lower_jump_if_else(
    test: lir::TempId,
    then_branch: lir::BlockId,
    else_branch: lir::BlockId,
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    let inst = Instruction::JumpIf(test.register(), 0);
    let inst_addr = chunk.len();
    let fill_back_addr = byte_code::OpCode::JUMP_IF.jump_distance_offset() + chunk.len();
    inst.emit(chunk);

    lower_block(
        else_branch,
        blocks,
        lir_functions,
        str_index,
        function_meta,
        chunk,
        functions,
        e,
    );
    let then_branch_addr = lower_block(
        then_branch,
        blocks,
        lir_functions,
        str_index,
        function_meta,
        chunk,
        functions,
        e,
    );

    fill_back_jump(
        then_branch_addr - inst_addr,
        fill_back_addr,
        chunk,
        function_meta,
        e,
    )
}

fn lower_jump_if_eq_int(
    test: lir::TempId,
    value: i64,
    branch: lir::BlockId,
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    let inst_addr = chunk.len();
    let fill_back_addr = if i32::MIN as i64 <= value && value <= i32::MAX as i64 {
        Instruction::JumpIfEqI32(test.register(), value as i32, 0).emit(chunk);
        inst_addr + byte_code::OpCode::JUMP_IF_EQ_I32.jump_distance_offset()
    } else {
        Instruction::JumpIfEqI64(test.register(), 0).emit(chunk);
        chunk.writer().push(value);
        inst_addr + byte_code::OpCode::JUMP_IF_EQ_I64.jump_distance_offset()
    };
    let branch_addr = lower_block(
        branch,
        blocks,
        lir_functions,
        str_index,
        function_meta,
        chunk,
        functions,
        e,
    );
    fill_back_jump(
        branch_addr - inst_addr,
        fill_back_addr,
        chunk,
        function_meta,
        e,
    );
}

fn lower_jump_if_eq_str(
    test: lir::TempId,
    value: ir::StrId,
    branch: lir::BlockId,
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    let inst = Instruction::JumpIfEqStr(test.register(), str_index[value], 0);
    let fill_back_addr = byte_code::OpCode::JUMP_IF_EQ_STR.jump_distance_offset() + chunk.len();

    lower_branch_and_fill_back(
        inst,
        fill_back_addr,
        branch,
        blocks,
        lir_functions,
        str_index,
        function_meta,
        chunk,
        functions,
        e,
    );
}

fn lower_jump_if_eq_bool(
    test: lir::TempId,
    value: bool,
    branch: lir::BlockId,
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    let inst = Instruction::JumpIfEqBool(test.register(), value, 0);
    let fill_back_addr = byte_code::OpCode::JUMP_IF_EQ_BOOL.jump_distance_offset() + chunk.len();

    lower_branch_and_fill_back(
        inst,
        fill_back_addr,
        branch,
        blocks,
        lir_functions,
        str_index,
        function_meta,
        chunk,
        functions,
        e,
    );
}

fn lower_jump(
    branch: lir::BlockId,
    blocks: &lir::BlockHeap,
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    function_meta: &Meta,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    lower_block(
        branch,
        blocks,
        lir_functions,
        str_index,
        function_meta,
        chunk,
        functions,
        e,
    );
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

fn lower_tail_call(
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
            1 => Instruction::TailCall1(ret, callee, collect_to_array(args)),
            2 => Instruction::TailCall2(ret, callee, collect_to_array(args)),
            3 => Instruction::TailCall3(ret, callee, collect_to_array(args)),
            4 => Instruction::TailCall4(ret, callee, collect_to_array(args)),
            5 => Instruction::TailCall5(ret, callee, collect_to_array(args)),
            _ => unreachable!(),
        };
        inst.emit(chunk)
    } else {
        Instruction::TailCallMany(ret, callee, args_len as u8).emit(chunk);
        args.fold(chunk.writer(), |writer, arg| writer.push(arg))
            .finish();
    }
}

fn lower_tail_call_u(
    ret: lir::TempId,
    callee: lir::FunctionId,
    args: &[lir::TempId],
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    let ret = ret.register();
    let callee = lower_function(callee, lir_functions, str_index, chunk, functions, e);
    let args_len = args.len();
    let args = args.iter().map(|a| a.register());

    if args_len <= 2 {
        let inst = match args_len {
            1 => Instruction::TailCallU1(ret, callee, collect_to_array(args)),
            2 => Instruction::TailCallU2(ret, callee, collect_to_array(args)),
            _ => unreachable!(),
        };
        inst.emit(chunk)
    } else {
        Instruction::TailCallUMany(ret, callee, args_len as u8).emit(chunk);
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
    let callee = callee.byte_code();
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
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    let ret = ret.register();
    let callee = lower_function(callee, lir_functions, str_index, chunk, functions, e);
    let args_len = args.len();
    let args = args.iter().map(|a| a.register());

    if args_len <= 2 {
        let inst = match args_len {
            1 => Instruction::Call1(ret, callee, collect_to_array(args)),
            2 => Instruction::Call2(ret, callee, collect_to_array(args)),
            _ => unreachable!(),
        };
        inst.emit(chunk)
    } else {
        Instruction::CallMany(ret, callee, args_len as u8).emit(chunk);
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
    lir_functions: &lir::FunctionList,
    str_index: &lir::StrIndex,
    chunk: &mut byte_code::Chunk,
    functions: &mut FunctionTable,
    e: &mut ManyError,
) {
    Instruction::Function(
        to.register(),
        lower_function(fid, lir_functions, str_index, chunk, functions, e),
    )
    .emit(chunk);
}

fn lower_builtin(to: lir::TempId, bid: lir::BuiltinId, chunk: &mut byte_code::Chunk) {
    Instruction::Builtin(to.register(), bid.byte_code()).emit(chunk);
}

fn lower_field(to: lir::TempId, from: lir::TempId, n: usize, chunk: &mut byte_code::Chunk) {
    Instruction::Field(to.register(), from.register(), n as u8).emit(chunk);
}

fn lower_panic(str_id: ir::StrId, str_index: &lir::StrIndex, chunk: &mut byte_code::Chunk) {
    Instruction::Panic(str_index[str_id]).emit(chunk)
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

fn lower_ret(from: lir::TempId, chunk: &mut byte_code::Chunk) {
    Instruction::Ret(from.register()).emit(chunk)
}
