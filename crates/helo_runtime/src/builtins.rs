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

fn int_add<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_int() + arg1.unwrap_int()))
}

fn int_subtract<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_int() - arg1.unwrap_int()))
}

fn int_mul<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_int() * arg1.unwrap_int()))
}

fn int_pow<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(
        arg0.unwrap_int().pow(
            u32::try_from(arg1.unwrap_int())
                .map_err(|_| RunTimeError::IntExponentOutOfRange(arg1.unwrap_int()))?,
        ),
    ))
}

fn int_div<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    let (a, b) = (arg0.unwrap_int(), arg1.unwrap_int());
    if b != 0 {
        Ok(ValueSafe::Int(a / b))
    } else {
        Err(RunTimeError::ZeroDivision)
    }
}

fn int_mod<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    let (a, b) = (arg0.unwrap_int(), arg1.unwrap_int());
    if b != 0 {
        Ok(ValueSafe::Int(a.rem_euclid(b)))
    } else {
        Err(RunTimeError::ZeroDivision)
    }
}

fn int_eq<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() == arg1.unwrap_int()))
}

fn int_ne<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() != arg1.unwrap_int()))
}

fn int_le<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() <= arg1.unwrap_int()))
}

fn int_ge<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() >= arg1.unwrap_int()))
}

fn int_lt<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() < arg1.unwrap_int()))
}

fn int_gt<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() > arg1.unwrap_int()))
}

fn int_to_float<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_int() as f64))
}

fn floor_float<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_float().floor() as i64))
}

fn ceil_float<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_float().ceil() as i64))
}

fn round_float<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_float().round() as i64))
}

fn float_add<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_float() + arg1.unwrap_float()))
}

fn float_subtract<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_float() - arg1.unwrap_float()))
}

fn float_mul<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_float() * arg1.unwrap_float()))
}

fn float_powi<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(
        arg0.unwrap_float().powi(
            i32::try_from(arg1.unwrap_int())
                .map_err(|_| RunTimeError::FloatExponentOutOfRange(arg1.unwrap_int()))?,
        ),
    ))
}

fn float_powf<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(
        arg0.unwrap_float().powf(arg1.unwrap_float()),
    ))
}

fn float_div<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_float() / arg1.unwrap_float()))
}

fn float_apr<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(
        (arg0.unwrap_float() - arg1.unwrap_float()).abs() <= 1e-6,
    ))
}

fn float_napr<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(
        (arg0.unwrap_float() - arg1.unwrap_float()).abs() > 1e-6,
    ))
}

fn float_le<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_float() <= arg1.unwrap_float()))
}

fn float_ge<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_float() >= arg1.unwrap_float()))
}

fn float_lt<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_float() < arg1.unwrap_float()))
}

fn float_gt<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_float() > arg1.unwrap_float()))
}

fn str_concat<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Obj(
        arg0.unwrap_obj()
            .cast::<mem::ObjString>()
            .concat(arg1.unwrap_obj().cast::<mem::ObjString>(), pool, lock)
            .map_err(|_| RunTimeError::OutOfMemory)?
            .cast_obj_ref(),
    ))
}

fn int_to_str<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    let s = arg0.unwrap_int().to_string();
    let s = pool
        .allocate_string(&s, lock)
        .map_err(|_| RunTimeError::OutOfMemory)?;
    Ok(ValueSafe::Obj(s.cast_obj_ref()))
}

fn float_to_str<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    let s = arg0.unwrap_float().to_string();
    let s = pool
        .allocate_string(&s, lock)
        .map_err(|_| RunTimeError::OutOfMemory)?;
    Ok(ValueSafe::Obj(s.cast_obj_ref()))
}

fn bool_to_string<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    let s = arg0.unwrap_bool().to_string();
    let s = pool
        .allocate_string(&s, lock)
        .map_err(|_| RunTimeError::OutOfMemory)?;
    Ok(ValueSafe::Obj(s.cast_obj_ref()))
}

fn char_to_string<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    let s = arg0
        .unwrap_obj()
        .cast::<mem::ObjChar>()
        .to_string(pool, lock);
    Ok(ValueSafe::Obj(s.cast_obj_ref()))
}

fn bool_not<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(!arg0.unwrap_bool()))
}

fn bool_and<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_bool() && arg1.unwrap_bool()))
}

fn bool_or<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_bool() || arg1.unwrap_bool()))
}

#[derive(Clone, Copy)]
pub enum Builtin {
    B1(BuiltinFunc<1>),
    B2(BuiltinFunc<2>),
    B3(BuiltinFunc<3>),
    B4(BuiltinFunc<4>),
}
use Builtin::*;

impl Builtin {
    pub fn arity(&self) -> usize {
        match self {
            B1(..) => 1,
            B2(..) => 2,
            B3(..) => 3,
            B4(..) => 4,
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

const BUILTINS: [(&'static str, Builtin); 36] = [
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
    ("+.", B2(float_add)),
    ("-.", B2(float_subtract)),
    ("*.", B2(float_mul)),
    ("**.", B2(float_powi)),
    ("**..", B2(float_powf)),
    ("/.", B2(float_div)),
    // Float comparison
    ("~=", B2(float_apr)),
    ("/~=", B2(float_napr)),
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
    ("str_cat", B2(str_concat)),
    // Bool arithmatic
    ("and", B2(bool_and)),
    ("or", B2(bool_or)),
    ("not", B1(bool_not)),
];

pub fn name_by_id(id: BuiltinId) -> &'static str {
    BUILTINS[id.0 as usize].0
}

pub fn get(id: BuiltinId) -> Builtin {
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
