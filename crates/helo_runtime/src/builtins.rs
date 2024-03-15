use crate::{
    errors, mem,
    vm::{self, VmIo},
};
use mem::ValueSafe;

use errors::RunTimeError;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BuiltinId(pub u16);

type BuiltinFunc<const N: usize, P> = for<'p> fn(
    [ValueSafe<'p>; N],
    pool: &mut mem::GcPool,
    registers: &mut mem::ValueVec,
    call_stack: &mut vm::CallStack,
    io: &mut P,
    lock: &'p mem::Lock,
) -> BuiltinRet<'p>;

type BuiltinRet<'p> = Result<ValueSafe<'p>, RunTimeError>;

mod functions;

#[derive(Clone, Copy)]
pub enum Builtin<P> {
    B0(BuiltinFunc<0, P>),
    B1(BuiltinFunc<1, P>),
    B2(BuiltinFunc<2, P>),
    B3(BuiltinFunc<3, P>),
    B4(BuiltinFunc<4, P>),
}
use Builtin::*;

impl<P> Builtin<P>
where
    P: VmIo,
{
    pub fn arity(&self) -> usize {
        match self {
            B0(..) => 0,
            B1(..) => 1,
            B2(..) => 2,
            B3(..) => 3,
            B4(..) => 4,
        }
    }
    pub fn unwrap0(&self) -> BuiltinFunc<0, P> {
        match self {
            B0(f) => *f,
            _ => panic!(
                "Type Error: Expected a builtin with 1 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap1(&self) -> BuiltinFunc<1, P> {
        match self {
            B1(f) => *f,
            _ => panic!(
                "Type Error: Expected a builtin with 1 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap2(&self) -> BuiltinFunc<2, P> {
        match self {
            B2(f) => *f,
            _ => panic!(
                "Type Error: Expected a builtin with 2 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap3(&self) -> BuiltinFunc<3, P> {
        match self {
            B3(f) => *f,
            _ => panic!(
                "Type Error: Expected a builtin with 3 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap4(&self) -> BuiltinFunc<4, P> {
        match self {
            B4(f) => *f,
            _ => panic!(
                "Type Error: Expected a builtin with 4 arguments, got {}",
                self.arity()
            ),
        }
    }
}

pub fn name_by_id(id: BuiltinId) -> String {
    format!(
        "{}.{}",
        BUILTIN_NAMES[id.0 as usize].0, BUILTIN_NAMES[id.0 as usize].1
    )
}

pub fn module_str(id: BuiltinId) -> &'static str {
    BUILTIN_NAMES[id.0 as usize].0
}

pub fn ident(id: BuiltinId) -> &'static str {
    BUILTIN_NAMES[id.0 as usize].1
}

pub fn get_arity(id: BuiltinId) -> usize {
    BUILTIN_NAMES[id.0 as usize].2
}

pub struct BuiltinTable<P>
where
    P: VmIo,
{
    tab: Vec<Builtin<P>>,
}

impl<P> BuiltinTable<P>
where
    P: VmIo,
{
    pub fn get(&self, id: BuiltinId) -> &Builtin<P> {
        &self.tab[id.0 as usize]
    }

    pub fn call_adapted<'p>(
        &self,
        id: BuiltinId,
        args: Vec<ValueSafe<'p>>,
        pool: &mut mem::GcPool,
        registers: &mut mem::ValueVec,
        call_stack: &mut vm::CallStack,
        io: &mut P,
        lock: &'p mem::Lock,
    ) -> BuiltinRet<'p> {
        let builtin = self.get(id);
        match builtin {
            B0(f) => f([], pool, registers, call_stack, io, lock),
            B1(f) => f([args[0]], pool, registers, call_stack, io, lock),
            B2(f) => f([args[0], args[1]], pool, registers, call_stack, io, lock),
            B3(f) => f(
                [args[0], args[1], args[2]],
                pool,
                registers,
                call_stack,
                io,
                lock,
            ),
            B4(f) => f(
                [args[0], args[1], args[2], args[3]],
                pool,
                registers,
                call_stack,
                io,
                lock,
            ),
        }
    }

    pub fn new<const ASYNC: bool>() -> Self {
        Self {
            tab: vec![
                // Int arithmatics
                B2(int_add),
                B2(int_subtract),
                B2(int_mul),
                B2(int_pow),
                B2(int_div),
                B2(int_mod),
                // Int comparison
                B2(int_eq),
                B2(int_ne),
                B2(int_ge),
                B2(int_le),
                B2(int_gt),
                B2(int_lt),
                // Float arithmatics
                B1(float_neg),
                B2(float_add),
                B2(float_subtract),
                B2(float_mul),
                B2(float_powi),
                B2(float_powf),
                B2(float_div),
                // Float comparison
                B2(float_apr),
                B2(float_napr),
                B2(float_ge),
                B2(float_le),
                B2(float_gt),
                B2(float_lt),
                // Float <-> Int
                B1(int_to_float),
                B1(floor_float),
                B1(ceil_float),
                B1(round_float),
                // To Str
                B1(int_to_str),
                B1(float_to_str),
                B1(bool_to_string),
                B1(char_to_string),
                // Bool arithmatic
                B2(bool_and),
                B2(bool_or),
                B1(bool_not),
                // Char
                B2(char_eq),
                // String operations
                B2(str_concat),
                B1(string_some),
                B1(string_len),
                B2(string_eq),
                B1(string_head),
                B1(string_tail),
                // Routines
                B1(string_println),
                B1(string_print),
                B0(read_line::<P, ASYNC>),
            ],
        }
    }

    pub fn new_sync() -> Self {
        Self::new::<false>()
    }

    pub fn new_async() -> Self {
        Self::new::<true>()
    }
}

use functions::*;
pub const BUILTIN_NAMES: [(&'static str, &'static str, usize); 46] = [
    // Int arithmatics
    ("arith.int", "+", 2),
    ("arith.int", "-", 2),
    ("arith.int", "*", 2),
    ("arith.int", "**", 2),
    ("arith.int", "/", 2),
    ("arith.int", "mod", 2),
    // Int comparison
    ("arith.int", "==", 2),
    ("arith.int", "/=", 2),
    ("arith.int", ">=", 2),
    ("arith.int", "<=", 2),
    ("arith.int", ">", 2),
    ("arith.int", "<", 2),
    // Float arithmatics
    ("arith.float", "neg", 1),
    ("arith.float", "+.", 2),
    ("arith.float", "-.", 2),
    ("arith.float", "*.", 2),
    ("arith.float", "**.", 2),
    ("arith.float", "**..", 2),
    ("arith.float", "/.", 2),
    // Float comparison
    ("arith.float", "=.", 2),
    ("arith.float", "/=.", 2),
    ("arith.float", ">=.", 2),
    ("arith.float", "<=.", 2),
    ("arith.float", ">.", 2),
    ("arith.float", "<.", 2),
    // Float <-> Int
    ("arith.float", "int_to_float", 1),
    ("arith.float", "floor_float", 1),
    ("arith.float", "ceil_float", 1),
    ("arith.float", "round_float", 1),
    // To Str
    ("fmt", "int_to_str", 1),
    ("fmt", "float_to_str", 1),
    ("fmt", "bool_to_str", 1),
    ("fmt", "char_to_str", 1),
    // Bool arithmatic
    ("arith.bool", "and", 2),
    ("arith.bool", "or", 2),
    ("arith.bool", "not", 1),
    // Char
    ("arith.char", "char_eq", 2),
    // String operations
    ("str", "str_cat", 2),
    ("str", "str_some", 1),
    ("str", "str_len", 1),
    ("str", "str_eq", 2),
    ("str", "str_head", 1),
    ("str", "str_tail", 1),
    // Routines
    ("io", "println", 1),
    ("io", "print", 1),
    ("io", "readline", 0),
];
