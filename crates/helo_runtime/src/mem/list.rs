use super::objects::{Gc, Obj, ObjPointer, ObjectKind, Pointer, Ref, StaticObjKind};
use super::value::{Value, ValueSafe};
use super::{GcPool, Lock};

use core::ptr;
use std::alloc;
use std::marker::PhantomData;

/// A list with each node garbage collected
/// # Safety
/// All pointers in this struct must be valid
pub struct ObjList {
    v: Option<Value>,
    next: Option<Pointer<ObjList>>,
    gc_marker: bool,
    next_obj: Option<ObjPointer>,
}

impl StaticObjKind for ObjList {
    fn kind() -> ObjectKind {
        ObjectKind::List
    }
}

impl Gc for ObjList {
    fn set_next_obj(&mut self, p: Option<ObjPointer>) {
        self.next_obj = p;
    }
    fn next_obj(&self) -> Option<ObjPointer> {
        self.next_obj
    }
    fn mark(&mut self) {
        if !self.gc_marker {
            self.gc_marker = true;
            if let Some(v) = &mut self.v {
                // Here we believe that `is_obj` ensures `v` is an object pointer;
                // And `ObjList` itself ensures `v` points to a valid object
                unsafe { v.mark() }
            }
            if let Some(next) = &mut self.next {
                // Here `ObjList` itself ensures `next` points to a valid object
                unsafe { next.mark() }
            }
        }
    }
    fn unmark(&mut self) {
        self.gc_marker = false;
    }
    fn is_marked(&self) -> bool {
        self.gc_marker
    }
}

impl std::fmt::Debug for ObjList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(unsafe { Pointer(self.into()).to_ref(PhantomData).iter() })
            .finish()
    }
}

impl Obj for ObjList {
    fn kind(&self) -> ObjectKind {
        ObjectKind::List
    }
}

impl ObjList {
    pub fn allocate() -> Result<Pointer<ObjList>, ()> {
        unsafe {
            let layout = alloc::Layout::new::<ObjList>();
            let ptr = alloc::alloc(layout) as *mut ObjList;
            if ptr.is_null() {
                return Err(());
            }

            let obj_list = &mut *ptr;
            obj_list.v = None;
            obj_list.next = None;
            obj_list.gc_marker = false;
            obj_list.next_obj = None;

            let trait_ptr = ptr::NonNull::new(ptr).unwrap();
            Ok(trait_ptr.into())
        }
    }
}

pub struct ListRefIter<'p>(Ref<'p, ObjList>);

impl<'p> Iterator for ListRefIter<'p> {
    type Item = ValueSafe<'p>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(head) = self.0.head() {
            self.0 = self.0.tail().unwrap();
            Some(head)
        } else {
            None
        }
    }
}

impl Pointer<ObjList> {
    pub unsafe fn con<'p>(
        self,
        v: Value,
        pool: &mut GcPool,
        lock: &Lock,
    ) -> Result<Pointer<ObjList>, ()> {
        let mut head = Pointer::from_ref(pool.allocate_list(lock)?);
        head.as_mut().v = Some(v);
        head.as_mut().next = Some(self);
        Ok(head)
    }
    pub unsafe fn head<'p>(self) -> Option<Value> {
        self.as_ref().v
    }
    pub unsafe fn tail<'p>(self) -> Option<Pointer<ObjList>> {
        self.as_ref().next
    }
}

impl<'p> Ref<'p, ObjList> {
    pub fn con(
        self,
        v: ValueSafe<'p>,
        pool: &mut GcPool,
        lock: &Lock,
    ) -> Result<Ref<'p, ObjList>, ()> {
        unsafe {
            Pointer::from_ref(self)
                .con(Value::from_safe(v), pool, lock)
                .map(|p| p.to_ref(PhantomData))
        }
    }
    pub fn head(self) -> Option<ValueSafe<'p>> {
        unsafe {
            let v = Pointer::from_ref(self).head();
            v.map(|v| v.to_safe(PhantomData))
        }
    }
    pub fn tail(self) -> Option<Ref<'p, ObjList>> {
        unsafe {
            let t = Pointer::from_ref(self).tail();
            t.map(|t| t.to_ref(PhantomData))
        }
    }
    pub fn iter(self) -> ListRefIter<'p> {
        ListRefIter(self)
    }
}
