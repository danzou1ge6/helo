mod gc;
mod value;

use std::marker::PhantomData;

pub use gc::{GcPool, ObjArray, ObjList, ObjRef, Ref};
pub use value::ValueSafe;

use self::value::Value;

pub struct ValueVec<'p> {
    v: Vec<value::ValueSafe<'p>>,
}

impl<'p> ValueVec<'p> {
    pub fn mark(mut self) -> ValueVecMarked {
        self.v.iter_mut().for_each(|v| v.mark());
        let v1 = self.v.into_iter().map(|v| Value::from_safe(v)).collect();
        ValueVecMarked { v: v1 }
    }
    pub fn read(&self, idx: usize) -> ValueSafe<'p> {
        self.v[idx]
    }
    pub fn write(&mut self, idx: usize, value: ValueSafe<'p>) {
        self.v[idx] = value
    }
    pub fn allocate(&mut self, cnt: usize) {
        (0..cnt).for_each(|_| self.v.push(ValueSafe::Int(0)));
    }
}

pub struct ValueVecMarked {
    v: Vec<value::Value>,
}

impl ValueVecMarked {
    unsafe fn to_unmarked<'p>(self, _phantom: PhantomData<&'p GcPool>) -> ValueVec<'p> {
        let v1 = self.v.into_iter().map(|v| v.to_safe(_phantom)).collect();
        ValueVec { v: v1 }
    }
}
