use crate::mem::value::value_debug_fmt_list;

use super::objects::{Gc, Obj, ObjDebug, ObjPointer, ObjectKind, Pointer, Ref, StaticObjKind};
use super::value::{Value, ValueSafe};

use smallvec::SmallVec;

use core::ptr;
use std::alloc;
use std::collections::HashSet;
use std::marker::PhantomData;

/// An array object
pub struct ObjArray {
    tag: u32,
    v: SmallVec<[Value; 4]>,
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

impl ObjDebug for ObjArray {
    fn debug_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        visited: &mut std::collections::HashSet<*const u8>,
    ) -> std::fmt::Result {
        visited.insert(Pointer(self.into()).addr());
        write!(f, "<Tag {} ", self.tag)?;
        unsafe {
            value_debug_fmt_list(self.v.iter().map(|v| v.to_safe(PhantomData)), f, visited)?;
        }
        write!(f, ">")
    }
}

impl std::fmt::Debug for ObjArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.debug_fmt(f, &mut HashSet::new())
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
            ptr::write(
                ptr::addr_of!(obj_arr.v) as *mut _,
                SmallVec::<[Value; 4]>::new(),
            );
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
