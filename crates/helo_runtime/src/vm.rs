use crate::errors;
use crate::{builtins, byte_code, executable, mem};
use byte_code::{Addr, ChunkReader, JumpDistance, RegisterId};
use executable::Executable;
use mem::ValueSafe;

pub struct CallFrame {
    register_offset: usize,
    return_addr: Addr,
    return_register: RegisterId,
}

impl std::fmt::Debug for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entry(&"register_offset", &self.register_offset)
            .entry(&"return_addr", &self.return_addr.to_string())
            .entry(&"return_register", &self.return_register.to_string())
            .finish()
    }
}

#[derive(Debug)]
pub struct CallStack {
    stack: Vec<CallFrame>,
    current_reg_cnt: usize,
}

impl CallStack {
    pub fn new() -> Self {
        Self {
            stack: vec![CallFrame {
                register_offset: 0,
                return_addr: 0.into(),
                return_register: RegisterId::default(),
            }],
            current_reg_cnt: 0,
        }
    }
    pub fn read_register<'p>(
        &self,
        vv: &mem::ValueVec,
        id: byte_code::RegisterId,
        lock: &'p mem::Lock,
    ) -> mem::ValueSafe<'p> {
        let offset = self.stack.last().unwrap().register_offset;
        vv.read(offset + id.0 as usize, lock)
    }
    pub fn read_register_of_last_frame<'p>(
        &self,
        vv: &mem::ValueVec,
        id: byte_code::RegisterId,
        lock: &'p mem::Lock,
    ) -> mem::ValueSafe<'p> {
        let offset = self.stack[self.stack.len() - 2].register_offset;
        vv.read(offset + id.0 as usize, lock)
    }
    pub fn write_register<'p>(
        &self,
        vv: &mut mem::ValueVec,
        id: byte_code::RegisterId,
        value: mem::ValueSafe<'p>,
    ) {
        let offset = self.stack.last().unwrap().register_offset;
        vv.write(offset + id.0 as usize, value)
    }
    pub fn new_frame<'p>(
        &mut self,
        vv: &mut mem::ValueVec,
        reg_cnt: usize,
        return_register: RegisterId,
        return_addr: Addr,
    ) {
        self.stack.push(CallFrame {
            register_offset: self.stack.last().unwrap().register_offset + self.current_reg_cnt,
            return_addr,
            return_register,
        });
        vv.allocate(reg_cnt);
        self.current_reg_cnt = reg_cnt;
    }
    pub fn pop_frame<'p>(&mut self, vv: &mut mem::ValueVec) -> Option<CallFrame> {
        vv.shrink(self.current_reg_cnt);
        let popped_frame = self.stack.pop().unwrap();

        if self.stack.len() == 1 {
            return None;
        }

        let old_offset = popped_frame.register_offset;
        let new_offset = self.stack.last().unwrap().register_offset;
        self.current_reg_cnt = old_offset - new_offset;

        Some(popped_frame)
    }
    pub fn return_frame<'p>(
        &mut self,
        vv: &mut mem::ValueVec,
        return_value: ValueSafe<'p>,
    ) -> Option<Addr> {
        let popped_frame = self.pop_frame(vv)?;
        self.write_register(vv, popped_frame.return_register, return_value);
        Some(popped_frame.return_addr)
    }
    pub fn return_frame_no_return_value<'p>(&mut self, vv: &mut mem::ValueVec) -> Option<Addr> {
        let popped_frame = self.pop_frame(vv)?;
        Some(popped_frame.return_addr)
    }
}

pub trait GcPolicy {
    fn if_do_gc(&self, memory_usage: usize) -> bool;
    fn update(&mut self, memory_usage: usize);
}

pub struct IncreasingGcPolicy {
    growth_rate: usize,
    next_gc: usize,
}

pub struct StressedGcPolicy {}

impl StressedGcPolicy {
    pub fn new() -> Self {
        Self {}
    }
}

impl GcPolicy for StressedGcPolicy {
    fn if_do_gc(&self, _memory_usage: usize) -> bool {
        true
    }
    fn update(&mut self, _memory_usage: usize) {}
}

impl GcPolicy for IncreasingGcPolicy {
    fn if_do_gc(&self, memory_usage: usize) -> bool {
        memory_usage > self.next_gc
    }
    fn update(&mut self, memory_usage: usize) {
        self.next_gc = self.growth_rate * memory_usage
    }
}

impl IncreasingGcPolicy {
    pub fn new(growth_rate: usize, first_gc: usize) -> Self {
        Self {
            growth_rate,
            next_gc: first_gc,
        }
    }
}

impl Default for IncreasingGcPolicy {
    fn default() -> Self {
        Self::new(2, 10 * 1024 * 1024)
    }
}

pub struct Vm<'e, G> {
    ip: Addr,
    exe: &'e Executable,
    gc_policy: G,
}

impl<'e, G> Vm<'e, G>
where
    G: GcPolicy,
{
    pub fn new(exe: &'e Executable, gc_policy: G) -> Self {
        Self {
            ip: exe.entry,
            exe,
            gc_policy,
        }
    }
    fn relative_jump(&mut self, offset: byte_code::JumpDistance) {
        self.ip += offset;
    }
    fn ip_inc_8bytes(&mut self) {
        self.ip += 8;
    }
    pub fn run(&mut self) -> Result<(mem::MemPack, mem::Lock), errors::RunTimeError> {
        let (mut pool, mut registers, mut lock) = mem::GcPool::new();
        let mut call_stack = CallStack::new();

        macro_rules! do_call_builtin {
            ($ret:expr, $builtin_id:expr, $args:expr, $pool:expr, $registers:expr, $call_stack:expr, $lock:expr, $f:ident) => {
                let values = $args.map(|arg| call_stack.read_register(&registers, arg, $lock));
                let builtin = builtins::get($builtin_id).$f();
                let return_value = builtin(values, $pool, $registers, $call_stack, $lock)?;
                call_stack.write_register(&mut registers, $ret, return_value);

                self.ip_inc_8bytes();
            };
        }

        self.do_call(
            &mut call_stack,
            &mut registers,
            RegisterId(0),
            self.ip,
            [],
            &lock,
        );

        loop {
            use byte_code::OpCode;
            use OpCode::*;

            let reader = self.exe.chunk.reader(self.ip);
            let (reader, op_code) = reader.read::<OpCode, _>();

            match op_code {
                JUMP_TABLE => {
                    self.do_jump_table(&mut call_stack, &mut registers, reader, &mut lock)
                }
                JUMP_IF => {
                    let (test, jump) = reader.jump_if();
                    if let ValueSafe::Bool(true) = call_stack.read_register(&registers, test, &lock)
                    {
                        self.relative_jump(jump);
                    } else {
                        self.ip_inc_8bytes();
                    }
                }
                JUMP_IF_EQ_BOOL => {
                    let (test, value, jump) = reader.jump_if_eq_bool();
                    if call_stack
                        .read_register(&registers, test, &lock)
                        .unwrap_bool()
                        == value
                    {
                        self.relative_jump(jump)
                    } else {
                        self.ip_inc_8bytes()
                    }
                }
                JUMP_IF_EQ_I32 => {
                    let (test, value, jump) = reader.jump_if_eq_i32();
                    if call_stack
                        .read_register(&registers, test, &lock)
                        .unwrap_int()
                        == value as i64
                    {
                        self.relative_jump(jump)
                    } else {
                        self.ip_inc_8bytes()
                    }
                }
                JUMP_IF_EQ_I64 => {
                    let (test, jump) = reader.jump_if_eq_i64();
                    let value = self.exe.chunk.read::<i64, _>(self.ip + 8);
                    if call_stack
                        .read_register(&registers, test, &lock)
                        .unwrap_int()
                        == value
                    {
                        self.relative_jump(jump)
                    } else {
                        self.ip_inc_8bytes();
                        self.ip_inc_8bytes();
                    }
                }
                JUMP_IF_EQ_STR => {
                    let (test, value, jump) = reader.jump_if_eq_str();
                    let value = self.exe.str_chunk.read(value);
                    let test_value = call_stack
                        .read_register(&registers, test, &lock)
                        .unwrap_obj()
                        .cast::<mem::ObjString>();
                    if test_value == *value {
                        self.relative_jump(jump);
                    } else {
                        self.ip_inc_8bytes();
                    }
                }
                JUMP_IF_EQ_CHAR => {
                    let (test, value, jump) = reader.jump_if_eq_char();
                    let test_value = call_stack
                        .read_register(&registers, test, &lock)
                        .unwrap_char();
                    if test_value == value {
                        self.relative_jump(jump);
                    } else {
                        self.ip_inc_8bytes();
                    }
                }
                JUMP => {
                    let offset = reader.jump();
                    self.relative_jump(offset);
                }
                APPLY1 => {
                    let (ret, callee, args) = reader.apply1();
                    self.do_apply(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        ret,
                        callee,
                        args,
                        &mut lock,
                    )?;
                }
                APPLY2 => {
                    let (ret, callee, args) = reader.apply2();
                    self.do_apply(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        ret,
                        callee,
                        args,
                        &mut lock,
                    )?;
                }
                APPLY3 => {
                    let (ret, callee, args) = reader.apply3();
                    self.do_apply(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        ret,
                        callee,
                        args,
                        &mut lock,
                    )?;
                }
                APPLY4 => {
                    let (ret, callee, args) = reader.apply4();
                    self.do_apply(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        ret,
                        callee,
                        args,
                        &mut lock,
                    )?;
                }
                APPLY5 => {
                    let (ret, callee, args) = reader.apply5();
                    self.do_apply(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        ret,
                        callee,
                        args,
                        &mut lock,
                    )?;
                }
                APPLY_MANY => {
                    let (ret, callee, cnt) = reader.apply_many();
                    self.do_apply_many(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        ret,
                        callee,
                        cnt,
                        &mut lock,
                    )?;
                }
                CALL1 => {
                    let (ret, f_addr, args) = reader.call1();
                    self.do_call(&mut call_stack, &mut registers, ret, f_addr, args, &lock);
                }
                CALL2 => {
                    let (ret, f_addr, args) = reader.call2();
                    self.do_call(&mut call_stack, &mut registers, ret, f_addr, args, &lock);
                }
                CALL_MANY => {
                    let (ret, f_addr, args_cnt) = reader.call_many();
                    self.do_call_many(
                        &mut call_stack,
                        &mut registers,
                        ret,
                        f_addr,
                        args_cnt,
                        &lock,
                    );
                }
                TAIL_CALL1 => {
                    let (f_addr, args) = reader.tail_call1();
                    self.do_tail_call(&mut call_stack, &mut registers, f_addr, args, &lock)
                }
                TAIL_CALL2 => {
                    let (f_addr, args) = reader.tail_call2();
                    self.do_tail_call(&mut call_stack, &mut registers, f_addr, args, &lock)
                }
                TAIL_CALL3 => {
                    let (f_addr, args) = reader.tail_call3();
                    self.do_tail_call(&mut call_stack, &mut registers, f_addr, args, &lock)
                }
                TAIL_CALL_MANY => {
                    let (f_addr, args_cnt) = reader.tail_call_many();
                    self.do_tail_call_many(&mut call_stack, &mut registers, f_addr, args_cnt, &lock)
                }
                TAIL_CALL_LOCAL1 => {
                    let (callee, args) = reader.tail_call_local1();
                    self.do_tail_call_local(&mut call_stack, &mut registers, callee, args, &lock)
                }
                TAIL_CALL_LOCAL2 => {
                    let (callee, args) = reader.tail_call_local2();
                    self.do_tail_call_local(&mut call_stack, &mut registers, callee, args, &lock)
                }
                TAIL_CALL_LOCAL3 => {
                    let (callee, args) = reader.tail_call_local3();
                    self.do_tail_call_local(&mut call_stack, &mut registers, callee, args, &lock)
                }
                TAIL_CALL_LOCAL4 => {
                    let (callee, args) = reader.tail_call_local4();
                    self.do_tail_call_local(&mut call_stack, &mut registers, callee, args, &lock)
                }
                TAIL_CALL_LOCAL5 => {
                    let (callee, args) = reader.tail_call_local5();
                    self.do_tail_call_local(&mut call_stack, &mut registers, callee, args, &lock)
                }
                TAIL_CALL_LOCAL6 => {
                    let (callee, args) = reader.tail_call_local6();
                    self.do_tail_call_local(&mut call_stack, &mut registers, callee, args, &lock)
                }
                TAIL_CALL_LOCAL_MANY => {
                    let (callee, args_cnt) = reader.tail_call_local_many();
                    self.do_tail_call_local_many(
                        &mut call_stack,
                        &mut registers,
                        callee,
                        args_cnt,
                        &lock,
                    );
                }
                INT32 => {
                    let (r, value) = reader.int32();
                    call_stack.write_register(&mut registers, r, ValueSafe::Int(value.into()));
                    self.ip_inc_8bytes();
                }
                INT64 => {
                    let r = reader.int64();
                    let value = self.exe.chunk.read::<i64, _>(self.ip + 8);
                    call_stack.write_register(&mut registers, r, ValueSafe::Int(value));
                    self.ip_inc_8bytes();
                    self.ip_inc_8bytes();
                }
                STR => {
                    let (r, value) = reader.str();
                    let value = self.exe.str_chunk.read(value);
                    let obj_string = pool
                        .allocate_string(value, &lock)
                        .map_err(|_| errors::RunTimeError::OutOfMemory {})?;
                    call_stack.write_register(
                        &mut registers,
                        r,
                        ValueSafe::Obj(obj_string.cast_obj_ref()),
                    );
                    self.ip_inc_8bytes();
                }
                FLOAT => {
                    let r = reader.int64();
                    let value = self.exe.chunk.read::<f64, _>(self.ip + 8);
                    call_stack.write_register(&mut registers, r, ValueSafe::Float(value));
                    self.ip_inc_8bytes();
                    self.ip_inc_8bytes();
                }
                CHAR => {
                    let (to, value) = reader.char();
                    call_stack.write_register(&mut registers, to, ValueSafe::Char(value));
                    self.ip_inc_8bytes();
                }
                BOOL => {
                    let (to, value) = reader.bool();
                    call_stack.write_register(&mut registers, to, ValueSafe::Bool(value));
                    self.ip_inc_8bytes();
                }
                ADD_TO_ENV1 => {
                    let (to, args) = reader.add_to_env1();
                    self.do_add_to_env(
                        &mut call_stack,
                        &mut registers,
                        to,
                        args,
                        &mut pool,
                        &lock,
                    )?;
                }
                ADD_TO_ENV2 => {
                    let (to, args) = reader.add_to_env2();
                    self.do_add_to_env(
                        &mut call_stack,
                        &mut registers,
                        to,
                        args,
                        &mut pool,
                        &lock,
                    )?;
                }
                ADD_TO_ENV3 => {
                    let (to, args) = reader.add_to_env3();
                    self.do_add_to_env(
                        &mut call_stack,
                        &mut registers,
                        to,
                        args,
                        &mut pool,
                        &lock,
                    )?;
                }
                ADD_TO_ENV4 => {
                    let (to, args) = reader.add_to_env4();
                    self.do_add_to_env(
                        &mut call_stack,
                        &mut registers,
                        to,
                        args,
                        &mut pool,
                        &lock,
                    )?;
                }
                ADD_TO_ENV5 => {
                    let (to, args) = reader.add_to_env5();
                    self.do_add_to_env(
                        &mut call_stack,
                        &mut registers,
                        to,
                        args,
                        &mut pool,
                        &lock,
                    )?;
                }
                ADD_TO_ENV6 => {
                    let (to, args) = reader.add_to_env6();
                    self.do_add_to_env(
                        &mut call_stack,
                        &mut registers,
                        to,
                        args,
                        &mut pool,
                        &lock,
                    )?;
                }
                PUSH1 => {
                    let (to, args) = reader.push1();
                    self.do_push(&mut call_stack, &mut registers, to, args, &lock);
                }
                PUSH2 => {
                    let (to, args) = reader.push2();
                    self.do_push(&mut call_stack, &mut registers, to, args, &lock);
                }
                PUSH3 => {
                    let (to, args) = reader.push3();
                    self.do_push(&mut call_stack, &mut registers, to, args, &lock);
                }
                PUSH4 => {
                    let (to, args) = reader.push4();
                    self.do_push(&mut call_stack, &mut registers, to, args, &lock);
                }
                PUSH5 => {
                    let (to, args) = reader.push5();
                    self.do_push(&mut call_stack, &mut registers, to, args, &lock);
                }
                PUSH6 => {
                    let (to, args) = reader.push6();
                    self.do_push(&mut call_stack, &mut registers, to, args, &lock);
                }
                FUNCTION => {
                    let (to, fid) = reader.function();
                    let arity = self.exe.chunk.read::<u32, _>(fid);
                    let routine = mem::Routine::User(fid);

                    let obj_callable = pool
                        .allocate_callable(routine, arity as usize, &lock)
                        .map(|obj| obj.cast_obj_ref())
                        .map_err(|_| errors::RunTimeError::OutOfMemory)?;
                    call_stack.write_register(&mut registers, to, ValueSafe::Obj(obj_callable));

                    self.ip_inc_8bytes();
                }
                BUILTIN => {
                    let (to, bid) = reader.builtin();
                    let arity = builtins::get_arity(bid);
                    let routine = mem::Routine::Builtin(bid);

                    let obj_callable = pool
                        .allocate_callable(routine, arity as usize, &lock)
                        .map(|obj| obj.cast_obj_ref())
                        .map_err(|_| errors::RunTimeError::OutOfMemory)?;
                    call_stack.write_register(&mut registers, to, ValueSafe::Obj(obj_callable));

                    self.ip_inc_8bytes();
                }
                FIELD => {
                    let (to, from, index) = reader.field();
                    let obj_array = call_stack
                        .read_register(&registers, from, &lock)
                        .unwrap_obj()
                        .cast::<mem::ObjArray>();

                    call_stack.write_register(
                        &mut registers,
                        to,
                        obj_array.get(index.into()).unwrap(),
                    );

                    self.ip_inc_8bytes();
                }
                TAGGED1 => {
                    let (to, tag, args) = reader.tagged1();
                    self.do_tagged(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        to,
                        tag,
                        args,
                        &lock,
                    )?;
                }
                TAGGED2 => {
                    let (to, tag, args) = reader.tagged2();
                    self.do_tagged(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        to,
                        tag,
                        args,
                        &lock,
                    )?;
                }
                TAGGED3 => {
                    let (to, tag, args) = reader.tagged3();
                    self.do_tagged(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        to,
                        tag,
                        args,
                        &lock,
                    )?;
                }
                TAGGED4 => {
                    let (to, tag, args) = reader.tagged4();
                    self.do_tagged(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        to,
                        tag,
                        args,
                        &lock,
                    )?;
                }
                TAGGED5 => {
                    let (to, tag, args) = reader.tagged5();
                    self.do_tagged(
                        &mut call_stack,
                        &mut registers,
                        &mut pool,
                        to,
                        tag,
                        args,
                        &lock,
                    )?;
                }
                TAGGED => {
                    let (to, tag) = reader.tagged();
                    let array = pool
                        .allocate_array(tag.into(), &lock)
                        .map_err(|_| errors::RunTimeError::OutOfMemory)?;

                    call_stack.write_register(
                        &mut registers,
                        to,
                        ValueSafe::Obj(array.cast_obj_ref()),
                    );

                    self.ip_inc_8bytes();
                }
                MOV => {
                    let (to, from) = reader.mov();
                    let value = call_stack.read_register(&registers, from, &lock);
                    call_stack.write_register(&mut registers, to, value);

                    self.ip_inc_8bytes();
                }
                PANIC => {
                    let msg_addr = reader.panic();
                    let reader = self.exe.chunk.reader(self.ip + 8);
                    let (reader, span0) = reader.read::<u32, _>();
                    let (reader, span1) = reader.read::<u32, _>();
                    let (_, file_addr) = reader.read::<byte_code::StrAddr, _>();

                    let msg = self.exe.str_chunk.read(msg_addr);
                    let file = self.exe.str_chunk.read(file_addr);

                    return Err(errors::RunTimeError::Panic {
                        msg: msg.to_string(),
                        file: file.to_string(),
                        span: (span0 as usize, span1 as usize),
                    });
                }
                RET => {
                    let reg = reader.ret();
                    let return_value = call_stack.read_register(&registers, reg, &lock);
                    if let Some(return_address) =
                        call_stack.return_frame(&mut registers, return_value)
                    {
                        self.ip = return_address;
                    } else {
                        return Ok((pool.pack(return_value), lock));
                    }
                }
                RET_NONE => {
                    if let Some(return_address) =
                        call_stack.return_frame_no_return_value(&mut registers)
                    {
                        self.ip = return_address;
                    } else {
                        return Ok((pool.pack(ValueSafe::Int(0)), lock));
                    }
                }
                CALL_BUILTIN1 => {
                    let (ret, builtin_id, args) = reader.call_builtin1();
                    do_call_builtin!(
                        ret,
                        builtin_id,
                        args,
                        &mut pool,
                        &mut registers,
                        &mut call_stack,
                        &lock,
                        unwrap1
                    );
                }
                CALL_BUILTIN2 => {
                    let (ret, builtin_id, args) = reader.call_builtin2();
                    do_call_builtin!(
                        ret,
                        builtin_id,
                        args,
                        &mut pool,
                        &mut registers,
                        &mut call_stack,
                        &lock,
                        unwrap2
                    );
                }
                CALL_BUILTIN3 => {
                    let (ret, builtin_id, args) = reader.call_builtin3();
                    do_call_builtin!(
                        ret,
                        builtin_id,
                        args,
                        &mut pool,
                        &mut registers,
                        &mut call_stack,
                        &lock,
                        unwrap3
                    );
                }
                CALL_BUILTIN4 => {
                    let (ret, builtin_id, args) = reader.call_builtin4();
                    do_call_builtin!(
                        ret,
                        builtin_id,
                        args,
                        &mut pool,
                        &mut registers,
                        &mut call_stack,
                        &lock,
                        unwrap4
                    );
                }
                UNKNOWN(x) => {
                    return Err(errors::RunTimeError::BadOpCode(x));
                }
            }

            if self.gc_policy.if_do_gc(pool.memory_usage()) {
                #[cfg(feature = "debug_gc")]
                {
                    println!("[ip = {}]", self.ip);
                    dbg!(&call_stack);
                    dbg!(&registers);
                }

                pool.sweep(&mut registers, &mut lock);
                self.gc_policy.update(pool.memory_usage());
            }
        }
    }

    fn do_tagged<'p, const N: usize>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        pool: &mut mem::GcPool,
        to: RegisterId,
        tag: u8,
        args: [RegisterId; N],
        lock: &'p mem::Lock,
    ) -> Result<(), errors::RunTimeError> {
        let mut array = pool
            .allocate_array(tag.into(), lock)
            .map_err(|_| errors::RunTimeError::OutOfMemory)?;
        args.into_iter().for_each(|arg| {
            array.push(call_stack.read_register(&registers, arg, lock));
        });
        call_stack.write_register(registers, to, ValueSafe::Obj(array.cast_obj_ref()));
        self.ip_inc_8bytes();
        Ok(())
    }

    fn do_push<'p, const N: usize>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        to: RegisterId,
        args: [RegisterId; N],
        lock: &'p mem::Lock,
    ) {
        let mut array = call_stack
            .read_register(&registers, to, lock)
            .unwrap_obj()
            .cast::<mem::ObjArray>();
        args.into_iter()
            .for_each(|arg| array.push(call_stack.read_register(registers, arg, lock)));
        self.ip_inc_8bytes();
    }

    fn do_add_to_env<'p, const N: usize>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        to: RegisterId,
        args: [RegisterId; N],
        pool: &mut mem::GcPool,
        lock: &'p mem::Lock,
    ) -> Result<(), errors::RunTimeError> {
        let callable = call_stack
            .read_register(&registers, to, lock)
            .unwrap_obj()
            .cast::<mem::ObjCallable>();
        callable
            .push_env_in_place(
                args.into_iter()
                    .map(|arg| call_stack.read_register(&registers, arg, lock)),
                pool,
                lock,
            )
            .map_err(|_| errors::RunTimeError::OutOfMemory {})?;
        self.ip_inc_8bytes();
        Ok(())
    }

    fn do_tail_call_local<'p, const N: usize>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        callee: RegisterId,
        args: [RegisterId; N],
        lock: &'p mem::Lock,
    ) {
        let callable = call_stack
            .read_register(registers, callee, lock)
            .unwrap_obj()
            .cast::<mem::ObjCallable>();

        match callable.routine() {
            mem::Routine::User(f_addr) => {
                let args_copied = args.map(|r| call_stack.read_register(registers, r, lock));
                for (i, arg) in (0..callable.env_len()).rev().zip(callable.env_iter()) {
                    call_stack.write_register(registers, RegisterId(i as u8), arg);
                }

                for (i, arg) in
                    (callable.env_len()..callable.env_len() + N).zip(args_copied.into_iter())
                {
                    call_stack.write_register(registers, RegisterId(i as u8), arg);
                }

                self.ip = f_addr + 8;
            }
            mem::Routine::Builtin(..) => unreachable!(),
        }
    }

    fn do_tail_call_local_many<'p>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        callee: RegisterId,
        args_cnt: u8,
        lock: &'p mem::Lock,
    ) {
        let callable = call_stack
            .read_register(registers, callee, lock)
            .unwrap_obj()
            .cast::<mem::ObjCallable>();

        match callable.routine() {
            mem::Routine::User(f_addr) => {
                let args_addr = self.ip + 8;

                let args_copied = self
                    .exe
                    .chunk
                    .fetch_registers(args_addr, args_cnt as usize)
                    .map(|r| call_stack.read_register(registers, r, lock))
                    .collect::<Vec<_>>();

                for (i, arg) in (0..callable.env_len()).rev().zip(callable.env_iter()) {
                    call_stack.write_register(registers, RegisterId(i as u8), arg);
                }

                for (i, arg) in (callable.env_len()..callable.env_len() + args_cnt as usize)
                    .zip(args_copied.into_iter())
                {
                    call_stack.write_register(registers, RegisterId(i as u8), arg);
                }

                self.ip = f_addr + 8;
            }
            mem::Routine::Builtin(..) => unreachable!(),
        }
    }

    fn do_tail_call<'p, const N: usize>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        f_addr: Addr,
        args: [RegisterId; N],
        lock: &'p mem::Lock,
    ) {
        let args_copies = args.map(|r| call_stack.read_register(registers, r, lock));
        for (i, arg) in args_copies.into_iter().enumerate() {
            call_stack.write_register(registers, RegisterId(i as u8), arg);
        }
        self.ip = f_addr + 8;
    }

    fn do_tail_call_many<'p>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        f_addr: Addr,
        args_cnt: u8,
        lock: &'p mem::Lock,
    ) {
        let args = self
            .exe
            .chunk
            .fetch_registers(self.ip + 8, args_cnt as usize)
            .map(|r| call_stack.read_register(registers, r, lock))
            .collect::<Vec<_>>();
        for (i, arg) in args.into_iter().enumerate() {
            call_stack.write_register(registers, RegisterId(i as u8), arg);
        }
        self.ip = f_addr + 8;
    }

    fn enter_function<'p>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        ret: RegisterId,
        f_addr: Addr,
        return_addr: Addr,
    ) {
        self.ip = f_addr;
        let reg_cnt = self.exe.chunk.read::<u32, _>(f_addr + 4);
        call_stack.new_frame(registers, reg_cnt as usize, ret, return_addr);
        self.ip += 8;
    }

    fn do_call<'p, const N: usize>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        ret: RegisterId,
        f_addr: Addr,
        args: [RegisterId; N],
        lock: &'p mem::Lock,
    ) {
        let return_addr = self.ip + 8;
        self.enter_function(call_stack, registers, ret, f_addr, return_addr);

        for (i, arg) in args.into_iter().enumerate() {
            call_stack.write_register(
                registers,
                RegisterId(i as u8),
                call_stack.read_register_of_last_frame(registers, arg, lock),
            );
        }
    }

    fn do_call_many<'p>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        ret: RegisterId,
        f_addr: Addr,
        args_cnt: u8,
        lock: &'p mem::Lock,
    ) {
        let return_addr = self.ip + 8 + Self::bytes_ceil_8(args_cnt.into());
        let args_addr = self.ip + 8;
        self.enter_function(call_stack, registers, ret, f_addr, return_addr);

        for (i, arg) in self
            .exe
            .chunk
            .fetch_registers(args_addr, args_cnt.into())
            .into_iter()
            .enumerate()
        {
            call_stack.write_register(
                registers,
                RegisterId(i as u8),
                call_stack.read_register_of_last_frame(registers, arg, lock),
            );
        }
    }

    fn do_jump_table<'p>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        reader: ChunkReader<'_, 1>,
        lock: &'p mem::Lock,
    ) {
        let (r, _) = reader.jump_table();
        let obj = call_stack.read_register(registers, r, lock).unwrap_obj();
        let arr = obj.cast::<mem::ObjArray>();
        let tag = arr.tag();
        let jump = self
            .exe
            .chunk
            .read(self.ip + 8 + tag * JumpDistance::bytes_len() as u32);
        self.relative_jump(jump);
    }

    fn do_apply<'p, const N: usize>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        pool: &mut mem::GcPool,
        ret: RegisterId,
        callee: RegisterId,
        args: [RegisterId; N],
        lock: &'p mem::Lock,
    ) -> Result<(), errors::RunTimeError> {
        let callable = call_stack
            .read_register(registers, callee, lock)
            .unwrap_obj()
            .cast::<mem::ObjCallable>();
        if callable.env_len() + N < callable.arity() {
            let applied = callable
                .push_env(
                    args.into_iter()
                        .map(|r| call_stack.read_register(registers, r, lock)),
                    pool,
                    lock,
                )
                .map_err(|_| errors::RunTimeError::OutOfMemory)?;
            call_stack.write_register(
                registers,
                ret,
                mem::ValueSafe::from_obj_ref(applied.cast_obj_ref()),
            );
            self.ip_inc_8bytes()
        } else {
            let routine = callable.routine();
            match routine {
                mem::Routine::User(f_addr) => {
                    let return_addr = self.ip + 8;
                    self.enter_function(call_stack, registers, ret, f_addr, return_addr);

                    for (i, arg) in (0..callable.env_len()).rev().zip(callable.env_iter()) {
                        call_stack.write_register(registers, RegisterId(i as u8), arg);
                    }
                    for (i, arg) in
                        (callable.env_len()..callable.env_len() + N).zip(args.into_iter())
                    {
                        call_stack.write_register(
                            registers,
                            RegisterId(i as u8),
                            call_stack.read_register_of_last_frame(registers, arg, lock),
                        );
                    }
                }
                mem::Routine::Builtin(builtin_id) => {
                    let mut args_sent = Vec::new();
                    for arg in callable.env_iter() {
                        args_sent.push(arg);
                    }
                    for arg in args {
                        args_sent.push(call_stack.read_register(registers, arg, lock));
                    }
                    let result = builtins::call_adapted(
                        builtin_id, args_sent, pool, registers, call_stack, lock,
                    )?;
                    call_stack.write_register(registers, ret, result);

                    self.ip_inc_8bytes();
                }
            }
        }
        Ok(())
    }

    fn bytes_ceil_8(cnt: usize) -> u32 {
        (cnt.div_ceil(8) * 8) as u32
    }
    fn ip_inc_bytes_ceil_8(&mut self, cnt: usize) {
        self.ip += Self::bytes_ceil_8(cnt)
    }

    fn do_apply_many<'p>(
        &mut self,
        call_stack: &mut CallStack,
        registers: &mut mem::ValueVec,
        pool: &mut mem::GcPool,
        ret: RegisterId,
        callee: RegisterId,
        args_cnt: u8,
        lock: &'p mem::Lock,
    ) -> Result<(), errors::RunTimeError> {
        let callable = call_stack
            .read_register(registers, callee, lock)
            .unwrap_obj()
            .cast::<mem::ObjCallable>();
        if callable.env_len() + (args_cnt as usize) < callable.arity() {
            self.ip_inc_8bytes();

            let applied = callable
                .push_env(
                    self.exe
                        .chunk
                        .fetch_registers(self.ip, args_cnt as usize)
                        .map(|reg| call_stack.read_register(registers, reg, lock)),
                    pool,
                    lock,
                )
                .map_err(|_| errors::RunTimeError::OutOfMemory)?;

            self.ip_inc_bytes_ceil_8(args_cnt.into());

            call_stack.write_register(
                registers,
                ret,
                mem::ValueSafe::from_obj_ref(applied.cast_obj_ref()),
            );
        } else {
            let routine = callable.routine();
            match routine {
                mem::Routine::User(f_addr) => {
                    let return_addr = self.ip + 8 + Self::bytes_ceil_8(args_cnt.into());
                    let args_addr = self.ip + 8;
                    self.enter_function(call_stack, registers, ret, f_addr, return_addr);

                    for (i, arg) in (0..callable.env_len()).rev().zip(callable.env_iter()) {
                        call_stack.write_register(registers, RegisterId(i as u8), arg);
                    }
                    for (i, arg) in (callable.env_len()..callable.env_len() + args_cnt as usize)
                        .zip(self.exe.chunk.fetch_registers(args_addr, args_cnt as usize))
                    {
                        call_stack.write_register(
                            registers,
                            RegisterId(i as u8),
                            call_stack.read_register_of_last_frame(registers, arg, lock),
                        );
                    }
                }
                mem::Routine::Builtin(builtin_id) => {
                    self.ip_inc_8bytes();
                    let mut args_sent = Vec::new();
                    for arg in callable.env_iter() {
                        args_sent.push(arg);
                    }
                    for arg in self.exe.chunk.fetch_registers(self.ip, args_cnt as usize) {
                        args_sent.push(call_stack.read_register(registers, arg, lock));
                    }
                    let result = builtins::call_adapted(
                        builtin_id, args_sent, pool, registers, call_stack, lock,
                    )?;
                    call_stack.write_register(registers, ret, result);

                    self.ip_inc_8bytes();
                }
            }
        }
        Ok(())
    }
}
