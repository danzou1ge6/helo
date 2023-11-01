use crate::{errors, mem, vm};
use mem::ValueSafe;
use std::collections::HashMap;

use errors::RunTimeError;

pub struct BuiltinTable {
    tab: HashMap<&'static str, BuiltinId>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BuiltinId(pub(crate) u16);

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
    BUILTINS[id.0 as usize].1
}

pub fn name(id: BuiltinId) -> &'static str {
    BUILTINS[id.0 as usize].0
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

impl BuiltinTable {
    pub fn new() -> Self {
        let mut tab = HashMap::new();
        for (i, (name, _)) in BUILTINS.iter().enumerate() {
            tab.insert(*name, BuiltinId(i as u16));
        }
        Self { tab }
    }
    pub fn id_by_name(&self, name: &str) -> BuiltinId {
        *self.tab.get(name).unwrap()
    }
    pub fn arity_by_name(&self, name: &str) -> usize {
        let id = self.tab.get(name).unwrap();
        get_arity(*id)
    }
}

use functions::*;
const BUILTINS: [(&'static str, Builtin); 45] = [
    // Int arithmatics
    ("+", B2(int_add)),
    ("-", B2(int_subtract)),
    ("*", B2(int_mul)),
    ("**", B2(int_pow)),
    ("/", B2(int_div)),
    ("mod", B2(int_mod)),
    // Int comparison
    ("==", B2(int_eq)),
    ("/=", B2(int_ne)),
    (">=", B2(int_ge)),
    ("<=", B2(int_le)),
    (">", B2(int_gt)),
    ("<", B2(int_lt)),
    // Float arithmatics
    ("neg", B1(float_neg)),
    ("+.", B2(float_add)),
    ("-.", B2(float_subtract)),
    ("*.", B2(float_mul)),
    ("**.", B2(float_powi)),
    ("**..", B2(float_powf)),
    ("/.", B2(float_div)),
    // Float comparison
    ("=.", B2(float_apr)),
    ("/=.", B2(float_napr)),
    (">=.", B2(float_ge)),
    ("<=.", B2(float_le)),
    (">.", B2(float_gt)),
    ("<.", B2(float_lt)),
    // Float <-> Int
    ("int_to_float", B1(int_to_float)),
    ("floor_float", B1(floor_float)),
    ("ceil_float", B1(ceil_float)),
    ("round_float", B1(round_float)),
    // To Str
    ("int_to_str", B1(int_to_str)),
    ("float_to_str", B1(float_to_str)),
    ("bool_to_str", B1(bool_to_string)),
    ("char_to_str", B1(char_to_string)),
    // Bool arithmatic
    ("and", B2(bool_and)),
    ("or", B2(bool_or)),
    ("not", B1(bool_not)),
    // Char
    ("char_eq", B2(char_eq)),
    // String operations
    ("str_cat", B2(str_concat)),
    ("str_some", B1(string_some)),
    ("str_len", B1(string_len)),
    ("str_eq", B2(string_eq)),
    ("str_head", B1(string_head)),
    ("str_tail", B1(string_tail)),
    // Routines
    ("println", B1(string_println)),
    ("readline", B0(read_line))
];
