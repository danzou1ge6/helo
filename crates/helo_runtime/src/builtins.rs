use crate::{errors, mem, vm};
use mem::ValueSafe;

use errors::RunTimeError;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BuiltinId(pub u16);

type BuiltinFunc<const N: usize> = for<'p> fn(
    [ValueSafe<'p>; N],
    pool: &mut mem::GcPool,
    registers: &mut mem::ValueVec,
    call_stack: &mut vm::CallStack,
    lock: &'p mem::Lock,
) -> BuiltinRet<'p>;

type BuiltinRet<'p> = Result<ValueSafe<'p>, RunTimeError>;

mod functions;

#[derive(Clone, Copy)]
pub enum Builtin {
    B0(BuiltinFunc<0>),
    B1(BuiltinFunc<1>),
    B2(BuiltinFunc<2>),
    B3(BuiltinFunc<3>),
    B4(BuiltinFunc<4>),
}
use Builtin::*;

impl Builtin {
    pub fn arity(&self) -> usize {
        match self {
            B0(..) => 0,
            B1(..) => 1,
            B2(..) => 2,
            B3(..) => 3,
            B4(..) => 4,
        }
    }
    pub fn unwrap0(self) -> BuiltinFunc<0> {
        match self {
            B0(f) => f,
            _ => panic!(
                "Type Error: Expected a builtin with 1 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap1(self) -> BuiltinFunc<1> {
        match self {
            B1(f) => f,
            _ => panic!(
                "Type Error: Expected a builtin with 1 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap2(self) -> BuiltinFunc<2> {
        match self {
            B2(f) => f,
            _ => panic!(
                "Type Error: Expected a builtin with 2 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap3(self) -> BuiltinFunc<3> {
        match self {
            B3(f) => f,
            _ => panic!(
                "Type Error: Expected a builtin with 3 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap4(self) -> BuiltinFunc<4> {
        match self {
            B4(f) => f,
            _ => panic!(
                "Type Error: Expected a builtin with 4 arguments, got {}",
                self.arity()
            ),
        }
    }
}

pub fn name_by_id(id: BuiltinId) -> &'static str {
    BUILTINS[id.0 as usize].0
}

pub fn get(id: BuiltinId) -> Builtin {
    BUILTINS[id.0 as usize].2
}

pub fn module_str(id: BuiltinId) -> &'static str {
    BUILTINS[id.0 as usize].0
}

pub fn ident(id: BuiltinId) -> &'static str {
    BUILTINS[id.0 as usize].1
}

pub fn get_arity(id: BuiltinId) -> usize {
    get(id).arity()
}

pub fn call_adapted<'p>(
    id: BuiltinId,
    args: Vec<ValueSafe<'p>>,
    pool: &mut mem::GcPool,
    registers: &mut mem::ValueVec,
    call_stack: &mut vm::CallStack,
    lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    let builtin = get(id);
    match builtin {
        B0(f) => f([], pool, registers, call_stack, lock),
        B1(f) => f([args[0]], pool, registers, call_stack, lock),
        B2(f) => f([args[0], args[1]], pool, registers, call_stack, lock),
        B3(f) => f(
            [args[0], args[1], args[2]],
            pool,
            registers,
            call_stack,
            lock,
        ),
        B4(f) => f(
            [args[0], args[1], args[2], args[3]],
            pool,
            registers,
            call_stack,
            lock,
        ),
    }
}

use functions::*;
pub const BUILTINS: [(&'static str, &'static str, Builtin); 46] = [
    // Int arithmatics
    ("arith.int", "+", B2(int_add)),
    ("arith.int", "-", B2(int_subtract)),
    ("arith.int", "*", B2(int_mul)),
    ("arith.int", "**", B2(int_pow)),
    ("arith.int", "/", B2(int_div)),
    ("arith.int", "mod", B2(int_mod)),
    // Int comparison
    ("arith.int", "==", B2(int_eq)),
    ("arith.int", "/=", B2(int_ne)),
    ("arith.int", ">=", B2(int_ge)),
    ("arith.int", "<=", B2(int_le)),
    ("arith.int", ">", B2(int_gt)),
    ("arith.int", "<", B2(int_lt)),
    // Float arithmatics
    ("arith.float", "neg", B1(float_neg)),
    ("arith.float", "+.", B2(float_add)),
    ("arith.float", "-.", B2(float_subtract)),
    ("arith.float", "*.", B2(float_mul)),
    ("arith.float", "**.", B2(float_powi)),
    ("arith.float", "**..", B2(float_powf)),
    ("arith.float", "/.", B2(float_div)),
    // Float comparison
    ("arith.float", "=.", B2(float_apr)),
    ("arith.float", "/=.", B2(float_napr)),
    ("arith.float", ">=.", B2(float_ge)),
    ("arith.float", "<=.", B2(float_le)),
    ("arith.float", ">.", B2(float_gt)),
    ("arith.float", "<.", B2(float_lt)),
    // Float <-> Int
    ("arith.float", "int_to_float", B1(int_to_float)),
    ("arith.float", "floor_float", B1(floor_float)),
    ("arith.float", "ceil_float", B1(ceil_float)),
    ("arith.float", "round_float", B1(round_float)),
    // To Str
    ("fmt", "int_to_str", B1(int_to_str)),
    ("fmt", "float_to_str", B1(float_to_str)),
    ("fmt", "bool_to_str", B1(bool_to_string)),
    ("fmt", "char_to_str", B1(char_to_string)),
    // Bool arithmatic
    ("arith.bool", "and", B2(bool_and)),
    ("arith.bool", "or", B2(bool_or)),
    ("arith.bool", "not", B1(bool_not)),
    // Char
    ("arith.char", "char_eq", B2(char_eq)),
    // String operations
    ("str", "str_cat", B2(str_concat)),
    ("str", "str_some", B1(string_some)),
    ("str", "str_len", B1(string_len)),
    ("str", "str_eq", B2(string_eq)),
    ("str", "str_head", B1(string_head)),
    ("str", "str_tail", B1(string_tail)),
    // Routines
    ("io", "println", B1(string_println)),
    ("io", "print", B1(string_print)),
    ("io", "readline", B0(read_line)),
];
