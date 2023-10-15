use super::list::{ListRefIter, ObjList};
use super::objects::{Gc, Obj, ObjPointer, ObjectKind, Pointer, Ref, StaticObjKind};
use super::value::ValueSafe;
use super::{GcPool, Lock};

use crate::{builtins, byte_code};

use core::ptr;
use std::alloc;
use std::marker::PhantomData;

#[derive(Clone, Copy)]
pub enum Routine {
    User(byte_code::Addr),
    Builtin(builtins::BuiltinId),
}

/// Wraps a function address and environment. Represents a closure or a curried function.
pub struct ObjCallable {
    f: Routine,
    arity: usize,
    env: Pointer<ObjList>,
    env_len: usize,
    gc_marker: bool,
    next_obj: Option<ObjPointer>,
}

impl Gc for ObjCallable {
    fn set_next_obj(&mut self, p: Option<ObjPointer>) {
        self.next_obj = p;
    }
    fn next_obj(&self) -> Option<ObjPointer> {
        self.next_obj
    }
    fn mark(&mut self) {
        self.gc_marker = true;
        unsafe { self.env.mark() };
    }
    fn unmark(&mut self) {
        self.gc_marker = false;
    }
    fn is_marked(&self) -> bool {
        self.gc_marker
    }
}

impl Obj for ObjCallable {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Callable
    }
}

impl StaticObjKind for ObjCallable {
    fn kind() -> ObjectKind {
        ObjectKind::Callable
    }
}

impl ObjCallable {
    pub(super) fn allocate_with_env(
        f: Routine,
        arity: usize,
        env: Pointer<ObjList>,
    ) -> Result<Pointer<ObjCallable>, ()> {
        unsafe {
            let layout = alloc::Layout::new::<ObjCallable>();
            let ptr = alloc::alloc(layout) as *mut ObjCallable;
            if ptr.is_null() {
                return Err(());
            }

            let obj_arr = &mut *ptr;
            *obj_arr = ObjCallable {
                f,
                arity,
                env_len: 0,
                env,
                gc_marker: false,
                next_obj: None,
            };

            let trait_ptr = ptr::NonNull::new(ptr).unwrap();
            Ok(trait_ptr.into())
        }
    }
    pub(super) fn allocate(f: Routine, arity: usize) -> Result<Pointer<ObjCallable>, ()> {
        let env = ObjList::allocate()?;
        Ok(ObjCallable::allocate_with_env(f, arity, env)?)
    }
}

impl Pointer<ObjCallable> {
    pub unsafe fn arity(self) -> usize {
        self.as_ref().arity
    }
    pub unsafe fn routine(self) -> Routine {
        self.as_ref().f
    }
    pub unsafe fn env(self) -> Pointer<ObjList> {
        self.as_ref().env
    }
}

impl<'p> Ref<'p, ObjCallable> {
    pub fn arity(self) -> usize {
        unsafe { Pointer::from_ref(self).arity() }
    }
    pub fn routine(self) -> Routine {
        unsafe { Pointer::from_ref(self).routine() }
    }
    fn env(self) -> Ref<'p, ObjList> {
        unsafe { Pointer::from_ref(self).env().to_ref(PhantomData) }
    }
    pub fn push_env(
        self,
        values: impl Iterator<Item = ValueSafe<'p>>,
        pool: &mut GcPool,
        lock: &Lock,
    ) -> Result<Ref<'p, ObjCallable>, ()> {
        let mut new_env = self.env();
        for value in values {
            new_env = new_env.con(value, pool, lock)?;
        }
        let new_env = Pointer::from_ref(new_env);
        let callable = ObjCallable::allocate_with_env(self.routine(), self.arity(), new_env)?;
        unsafe { Ok(callable.to_ref(PhantomData)) }
    }
    pub fn env_iter(&self) -> ListRefIter<'p> {
        self.env().iter()
    }
    pub fn env_len(&self) -> usize {
        self.as_ref().env_len
    }
}
