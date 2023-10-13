mod gc;
mod value;

use std::marker::PhantomData;

pub use gc::{GcPool, Lock, ObjArray, ObjCallable, ObjList, ObjRef, Ref, Routine};
pub use value::ValueSafe;

use self::value::Value;

/// # Safety:
/// All pointer should be valid
pub struct ValueVec {
    v: Vec<value::Value>,
}

impl ValueVec {
    fn new() -> Self {
        Self { v: Vec::new() }
    }
    fn mark(&mut self) {
        unsafe {
            self.v.iter_mut().for_each(|v| v.mark());
        }
    }
    pub fn read<'p>(&self, idx: usize, lock: &'p Lock) -> ValueSafe<'p> {
        unsafe { self.v[idx].to_safe(PhantomData) }
    }
    pub fn write<'p>(&mut self, idx: usize, value: ValueSafe<'p>) {
        self.v[idx] = Value::from_safe(value)
    }
    pub fn allocate(&mut self, cnt: usize) {
        (0..cnt).for_each(|_| self.v.push(Value::default()));
    }
    pub fn shrink(&mut self, cnt: usize) {
        self.v.resize_with(self.v.len() - cnt, || Value::default())
    }
}
