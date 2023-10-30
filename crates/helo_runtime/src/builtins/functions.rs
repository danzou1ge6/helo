use crate::{errors, mem, vm};
use errors::RunTimeError;
use mem::ValueSafe;

use super::BuiltinRet;

pub fn int_add<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_int() + arg1.unwrap_int()))
}

pub fn int_subtract<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_int() - arg1.unwrap_int()))
}

pub fn int_mul<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_int() * arg1.unwrap_int()))
}

pub fn int_pow<'p>(
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

pub fn int_div<'p>(
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

pub fn int_mod<'p>(
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

pub fn int_eq<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() == arg1.unwrap_int()))
}

pub fn int_ne<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() != arg1.unwrap_int()))
}

pub fn int_le<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() <= arg1.unwrap_int()))
}

pub fn int_ge<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() >= arg1.unwrap_int()))
}

pub fn int_lt<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() < arg1.unwrap_int()))
}

pub fn int_gt<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_int() > arg1.unwrap_int()))
}

pub fn int_to_float<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_int() as f64))
}

pub fn floor_float<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_float().floor() as i64))
}

pub fn ceil_float<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_float().ceil() as i64))
}

pub fn round_float<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(arg0.unwrap_float().round() as i64))
}

pub fn float_neg<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(-arg0.unwrap_float()))
}

pub fn float_add<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_float() + arg1.unwrap_float()))
}

pub fn float_subtract<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_float() - arg1.unwrap_float()))
}

pub fn float_mul<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_float() * arg1.unwrap_float()))
}

pub fn float_powi<'p>(
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

pub fn float_powf<'p>(
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

pub fn float_div<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Float(arg0.unwrap_float() / arg1.unwrap_float()))
}

pub fn float_apr<'p>(
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

pub fn float_napr<'p>(
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

pub fn float_le<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_float() <= arg1.unwrap_float()))
}

pub fn float_ge<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_float() >= arg1.unwrap_float()))
}

pub fn float_lt<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_float() < arg1.unwrap_float()))
}

pub fn float_gt<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_float() > arg1.unwrap_float()))
}

pub fn str_concat<'p>(
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

pub fn int_to_str<'p>(
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

pub fn float_to_str<'p>(
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

pub fn bool_to_string<'p>(
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

pub fn char_to_string<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    let s = arg0.unwrap_char().to_string();
    let s = pool
        .allocate_string(&s, lock)
        .map_err(|_| RunTimeError::OutOfMemory)?;
    Ok(ValueSafe::Obj(s.cast_obj_ref()))
}

pub fn bool_not<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(!arg0.unwrap_bool()))
}

pub fn bool_and<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_bool() && arg1.unwrap_bool()))
}

pub fn bool_or<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_bool() || arg1.unwrap_bool()))
}

pub fn char_eq<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(arg0.unwrap_char() == arg1.unwrap_char()))
}

pub fn string_some<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(
        arg0.unwrap_obj().cast::<mem::ObjString>().non_empty(),
    ))
}

pub fn string_len<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Int(
        arg0.unwrap_obj().cast::<mem::ObjString>().len() as i64,
    ))
}

pub fn string_eq<'p>(
    [arg0, arg1]: [ValueSafe<'p>; 2],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Bool(
        arg0.unwrap_obj()
            .cast::<mem::ObjString>()
            .eq(&arg1.unwrap_obj().cast::<mem::ObjString>()),
    ))
}

pub fn string_head<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Char(
        arg0.unwrap_obj()
            .cast::<mem::ObjString>()
            .head()
            .map_or(Err(RunTimeError::EmptyString), |x| Ok(x))?,
    ))
}

pub fn string_tail<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    Ok(ValueSafe::Obj(
        arg0.unwrap_obj()
            .cast::<mem::ObjString>()
            .tail(pool, lock)
            .map_or(Err(RunTimeError::EmptyString), |x| Ok(x.cast_obj_ref()))?,
    ))
}

pub fn string_println<'p>(
    [arg0]: [ValueSafe<'p>; 1],
    _pool: &mut mem::GcPool,
    _registers: &mut mem::ValueVec,
    _call_stack: &mut vm::CallStack,
    _lock: &'p mem::Lock,
) -> BuiltinRet<'p> {
    let s = arg0.unwrap_obj().cast::<mem::ObjString>();
    println!("{}", s.as_ref());
    Ok(ValueSafe::Int(0))
}

