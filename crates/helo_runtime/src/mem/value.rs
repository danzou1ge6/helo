use std::marker::PhantomData;

use super::{
    objects::{self, ObjDebug},
    GcPool, ObjPointer,
};
use objects::{Obj, ObjRef, Pointer};

/// A cheap to copy value in registers.
/// `Obj` does not guarantee a valid pointer
#[derive(Clone, Copy)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Obj(Pointer<dyn Obj>),
}

impl Default for Value {
    fn default() -> Self {
        Value::Int(0)
    }
}

impl<'p> std::fmt::Debug for ValueSafe<'p> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ValueSafe::*;
        match self {
            Int(i) => write!(f, "{i}"),
            Float(i) => write!(f, "{i}",),
            Bool(b) => write!(f, "{b}"),
            Char(c) => write!(f, "{c}"),
            Obj(obj) => write!(f, "{:?}{:?}", obj.addr(), obj.as_ref()),
        }
    }
}

/// Same as `Value`, but the lifetime '`p` guarantees that `Obj` variant points to a valid object.
#[derive(Clone, Copy)]
pub enum ValueSafe<'p> {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Obj(ObjRef<'p>),
}

pub trait ToSafe {
    type Output<'p>;
    unsafe fn to_safe<'p>(self, _phantom: PhantomData<&'p GcPool>) -> Self::Output<'p>;
    fn from_safe<'p>(v: Self::Output<'p>) -> Self;
}

impl ToSafe for Value {
    type Output<'p> = ValueSafe<'p>;
    unsafe fn to_safe<'p>(self, _phantom: PhantomData<&'p GcPool>) -> Self::Output<'p> {
        self.to_safe(_phantom)
    }
    fn from_safe<'p>(v: Self::Output<'p>) -> Self {
        Self::from_safe(v)
    }
}

impl ToSafe for Vec<Value> {
    type Output<'p> = Vec<ValueSafe<'p>>;
    unsafe fn to_safe<'p>(self, _phantom: PhantomData<&'p GcPool>) -> Self::Output<'p> {
        self.into_iter().map(|v| v.to_safe(_phantom)).collect()
    }
    fn from_safe<'p>(v: Self::Output<'p>) -> Self {
        v.into_iter().map(|v| Value::from_safe(v)).collect()
    }
}

impl Value {
    pub fn from_safe<'p>(v: ValueSafe<'p>) -> Self {
        use ValueSafe::*;
        match v {
            Int(i) => Value::Int(i),
            Float(f) => Value::Float(f),
            Bool(b) => Value::Bool(b),
            Char(c) => Value::Char(c),
            Obj(o) => Value::Obj(Pointer::from_ref(o)),
        }
    }
    pub unsafe fn to_safe<'p>(self, _phantom: PhantomData<&'p GcPool>) -> ValueSafe<'p> {
        use Value::*;
        match self {
            Int(i) => ValueSafe::Int(i),
            Float(f) => ValueSafe::Float(f),
            Bool(b) => ValueSafe::Bool(b),
            Char(c) => ValueSafe::Char(c),
            Obj(o) => ValueSafe::Obj(o.to_ref(_phantom)),
        }
    }
    pub unsafe fn mark(&mut self) {
        match self {
            Value::Obj(o) => o.mark(),
            _ => {}
        }
    }
    pub unsafe fn get_obj(self) -> Option<ObjPointer> {
        match self {
            Value::Obj(p) => Some(p),
            _ => None,
        }
    }
}

impl<'p> ObjDebug for ValueSafe<'p> {
    fn debug_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        visited: &mut std::collections::HashSet<*const u8>,
    ) -> std::fmt::Result {
        use ValueSafe::*;
        match self {
            Int(i) => write!(f, "{i}"),
            Float(i) => write!(f, "{i}",),
            Bool(b) => write!(f, "{b}"),
            Char(c) => write!(f, "'{c}'"),
            Obj(obj) => {
                if !visited.contains(&obj.addr()) {
                    visited.insert(obj.addr());
                    write!(f, "{:?}{:?}", obj.addr(), obj.as_ref())?;
                } else {
                    write!(f, "{:?}", obj.addr())?;
                }
                Ok(())
            }
        }
    }
}

pub fn value_debug_fmt_list<'p>(
    list: impl Iterator<Item = ValueSafe<'p>>,
    f: &mut std::fmt::Formatter<'_>,
    visited: &mut std::collections::HashSet<*const u8>,
) -> std::fmt::Result {
    write!(f, "[")?;
    for v in list {
        v.debug_fmt(f, visited)?;
        write!(f, ", ")?;
    }
    write!(f, "]")
}

impl<'p> ValueSafe<'p> {
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
    pub fn unwrap_char(self) -> char {
        if let Self::Char(i) = self {
            i
        } else {
            panic!("Type Exception: Unwrapping a Value of type Char")
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
