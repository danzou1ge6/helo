use super::objects::{Gc, Obj, ObjPointer, ObjectKind, Pointer, Ref, StaticObjKind};
use super::value::{Value, ValueSafe};

use core::ptr;
use std::alloc;
use std::marker::PhantomData;

/// An array object
pub struct ObjArray {
    tag: u32,
    v: Vec<Value>,
    gc_marker: bool,
    next_obj: Option<ObjPointer>,
}

impl StaticObjKind for ObjArray {
    fn kind() -> ObjectKind {
        ObjectKind::Array
    }
}

impl Gc for ObjArray {
    fn set_next_obj(&mut self, p: Option<ObjPointer>) {
        self.next_obj = p
    }
    fn next_obj(&self) -> Option<ObjPointer> {
        self.next_obj
    }
    fn mark(&mut self) {
        if !self.gc_marker {
            self.gc_marker = true;
            self.v.iter_mut().for_each(|v| unsafe { v.mark() });
        }
    }
    fn unmark(&mut self) {
        self.gc_marker = false;
    }
    fn is_marked(&self) -> bool {
        self.gc_marker
    }
}

impl std::fmt::Debug for ObjArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Tag {} ", self.tag)?;
        f.debug_list()
            .entries(self.v.iter().map(|v| unsafe { v.to_safe(PhantomData) }))
            .finish()?;
        write!(f, ">")
    }
}

impl Obj for ObjArray {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Array
    }
}

impl ObjArray {
    pub fn allocate(tag: u32) -> Result<Pointer<ObjArray>, ()> {
        unsafe {
            let layout = alloc::Layout::new::<ObjArray>();
            let ptr = alloc::alloc(layout) as *mut ObjArray;
            if ptr.is_null() {
                return Err(());
            }

            let obj_arr = &mut *ptr;
            obj_arr.tag = tag;
            ptr::write(ptr::addr_of!(obj_arr.v) as *mut _, Vec::<Value>::new());
            obj_arr.gc_marker = false;
            obj_arr.next_obj = None;

            let trait_ptr = ptr::NonNull::new_unchecked(ptr);
            Ok(trait_ptr.into())
        }
    }
}

impl Pointer<ObjArray> {
    pub unsafe fn tag(&self) -> u32 {
        self.as_ref().tag
    }
    pub unsafe fn push(&mut self, v: Value) {
        self.as_mut().v.push(v)
    }
    pub unsafe fn get(&self, idx: usize) -> Option<Value> {
        self.as_ref().v.get(idx).copied()
    }
}

impl<'p> Ref<'p, ObjArray> {
    pub fn tag(&self) -> u32 {
        unsafe { Pointer::from_ref(*self).tag() }
    }
    pub fn push(&mut self, v: ValueSafe<'p>) {
        unsafe { Pointer::from_ref(*self).push(Value::from_safe(v)) }
    }
    pub fn get(&self, idx: usize) -> Option<ValueSafe<'p>> {
        unsafe {
            Pointer::from_ref(*self)
                .get(idx)
                .map(|v| v.to_safe(PhantomData))
        }
    }
}
