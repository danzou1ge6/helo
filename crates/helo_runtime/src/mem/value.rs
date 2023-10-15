use std::marker::PhantomData;

use super::{objects, GcPool};
use objects::{Obj, ObjRef, Pointer};

/// A cheap to copy value in registers.
/// `Obj` does not guarantee a valid pointer
#[derive(Clone, Copy)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Obj(Pointer<dyn Obj>),
}

impl Default for Value {
    fn default() -> Self {
        Value::Int(0)
    }
}

/// Same as `Value`, but the lifetime '`p` guarantees that `Obj` variant points to a valid object.
#[derive(Clone, Copy, Debug)]
pub enum ValueSafe<'p> {
    Int(i64),
    Float(f64),
    Bool(bool),
    Obj(ObjRef<'p>),
}

impl Value {
    pub fn from_safe<'p>(v: ValueSafe<'p>) -> Self {
        use ValueSafe::*;
        match v {
            Int(i) => Value::Int(i),
            Float(f) => Value::Float(f),
            Bool(b) => Value::Bool(b),
            Obj(o) => Value::Obj(Pointer::from_ref(o)),
        }
    }
    pub unsafe fn to_safe<'p>(self, _phantom: PhantomData<&'p GcPool>) -> ValueSafe<'p> {
        use Value::*;
        match self {
            Int(i) => ValueSafe::Int(i),
            Float(f) => ValueSafe::Float(f),
            Bool(b) => ValueSafe::Bool(b),
            Obj(o) => ValueSafe::Obj(o.to_ref(_phantom)),
        }
    }
    pub unsafe fn mark(&mut self) {
        match self {
            Value::Obj(o) => o.mark(),
            _ => {}
        }
    }
}

impl<'p> ValueSafe<'p> {
    pub fn mark(&mut self) {
        match self {
            ValueSafe::Obj(o) => o.mark(),
            _ => {}
        }
    }
    pub fn unwrap_int(self) -> i64 {
        if let Self::Int(i) = self {
            i
        } else {
            panic!("Type Exception: Unwrapping a Value of type Int")
        }
    }
    pub fn unwrap_float(self) -> f64 {
        if let Self::Float(i) = self {
            i
        } else {
            panic!("Type Exception: Unwrapping a Value of type Float")
        }
    }
    pub fn unwrap_bool(self) -> bool {
        if let Self::Bool(i) = self {
            i
        } else {
            panic!("Type Exception: Unwrapping a Value of type Bool")
        }
    }
    pub fn unwrap_obj(self) -> ObjRef<'p> {
        if let Self::Obj(i) = self {
            i
        } else {
            panic!("Type Exception: Unwrapping a Value of type Obj")
        }
    }
    pub fn from_obj_ref(r: objects::ObjRef<'p>) -> Self {
        Self::Obj(r)
    }
}

impl<'p> Default for ValueSafe<'p> {
    fn default() -> Self {
        Self::Int(0)
    }
}
