use super::{
    value::Value, ObjArray, ObjCallable, ObjList, ObjString, Routine, ValueSafe, ValueVec,
};

use core::ptr;
use std::{alloc, marker::PhantomData, ptr::NonNull};

pub struct Pointer<T>(pub(super) ptr::NonNull<T>)
where
    T: ?Sized + Obj;

impl<T> Clone for Pointer<T>
where
    T: ?Sized + Obj,
{
    fn clone(&self) -> Self {
        Pointer(self.0.clone())
    }
}
impl<T> Copy for Pointer<T> where T: ?Sized + Obj {}

impl<T> From<ptr::NonNull<T>> for Pointer<T>
where
    T: ?Sized + Obj,
{
    fn from(value: ptr::NonNull<T>) -> Self {
        Self(value)
    }
}

impl<T> Pointer<T>
where
    T: Obj + 'static,
{
    pub unsafe fn deallocate(self) {
        let layout = alloc::Layout::new::<T>();
        alloc::dealloc(self.0.as_ptr() as *mut u8, layout);
    }
    fn cast_obj_pointer(self) -> ObjPointer {
        unsafe {
            let ptr = self.0.as_ptr() as *mut dyn Obj;
            let ptr = ptr::NonNull::new_unchecked(ptr);
            Pointer(ptr)
        }
    }
}

pub type ObjPointer = Pointer<dyn Obj>;

impl ObjPointer {
    pub unsafe fn deallocate(self) {
        let layout = match self.kind() {
            ObjectKind::Array => alloc::Layout::new::<ObjArray>(),
            ObjectKind::List => alloc::Layout::new::<ObjList>(),
            ObjectKind::Callable => alloc::Layout::new::<ObjCallable>(),
            ObjectKind::String => alloc::Layout::new::<ObjString>(),
        };
        std::ptr::drop_in_place(self.0.as_ptr());
        alloc::dealloc(self.0.as_ptr() as *mut u8, layout);
    }
    pub unsafe fn size(self) -> usize {
        match self.kind() {
            ObjectKind::Array => std::mem::size_of::<ObjArray>(),
            ObjectKind::List => std::mem::size_of::<ObjList>(),
            ObjectKind::Callable => std::mem::size_of::<ObjCallable>(),
            ObjectKind::String => std::mem::size_of::<ObjString>(),
        }
    }
}

impl<T> Pointer<T>
where
    T: ?Sized + Obj,
{
    pub unsafe fn set_next_obj(&mut self, p: Option<ObjPointer>) {
        self.0.as_mut().set_next_obj(p)
    }
    pub unsafe fn next_obj(&self) -> Option<ObjPointer> {
        self.0.as_ref().next_obj()
    }
    pub unsafe fn mark(&mut self) {
        #[cfg(feature = "debug_gc")]
        println!("Marked [{:?}] {:?}", self.0, self.as_ref());
        self.0.as_mut().mark();
    }
    pub unsafe fn unmark(&mut self) {
        #[cfg(feature = "debug_gc")]
        println!("UnMarked [{:?}] {:?}", self.0, self.as_ref());
        self.0.as_mut().unmark();
    }
    pub unsafe fn is_marked(&self) -> bool {
        self.0.as_ref().is_marked()
    }
    pub fn from_ref<'p>(r: Ref<'p, T>) -> Self {
        Self(r.p)
    }
    pub unsafe fn to_ref<'p>(self, _phantom: PhantomData<&'p GcPool>) -> Ref<'p, T> {
        Ref {
            p: self.0,
            _m: PhantomData,
        }
    }
    pub unsafe fn as_ref(&self) -> &T {
        self.0.as_ref()
    }
    pub unsafe fn as_mut(&mut self) -> &mut T {
        self.0.as_mut()
    }
}

impl<'p, T> Ref<'p, T>
where
    T: Obj + ?Sized,
{
    pub fn mark(&mut self) {
        unsafe { self.p.as_mut().mark() }
    }
    pub fn kind(&self) -> ObjectKind {
        unsafe { self.p.as_ref().kind() }
    }
    pub fn as_ref(&self) -> &T {
        unsafe { self.p.as_ref() }
    }
    pub fn as_mut(&mut self) -> &mut T {
        unsafe { self.p.as_mut() }
    }
    pub fn addr(&self) -> NonNull<T> {
        self.p
    }
}

impl<'p, T> Ref<'p, T>
where
    T: Obj + 'static,
{
    pub fn cast_obj_ref(self) -> ObjRef<'p> {
        unsafe {
            Pointer::from_ref(self)
                .cast_obj_pointer()
                .to_ref(PhantomData)
        }
    }
}

impl<'p> ObjRef<'p> {
    pub fn cast<T>(self) -> Ref<'p, T>
    where
        T: Obj + StaticObjKind,
    {
        unsafe { Pointer::from_ref(self).cast().to_ref(PhantomData) }
    }
}

impl ObjPointer {
    pub unsafe fn kind(&self) -> ObjectKind {
        self.0.as_ref().kind()
    }
    pub unsafe fn cast_unchecked<T>(self) -> Pointer<T>
    where
        T: Obj,
    {
        Pointer(self.0.cast::<T>())
    }
    pub unsafe fn cast<T>(self) -> Pointer<T>
    where
        T: Obj + StaticObjKind,
    {
        if <T as StaticObjKind>::kind() == self.kind() {
            self.cast_unchecked::<T>()
        } else {
            panic!(
                "Cast Obj Error: Casting to {:?}, got {:?}",
                <T as StaticObjKind>::kind(),
                self.kind()
            )
        }
    }
}

/// A always valid pointer to some object.
/// The `'p` lifetime marker here is from [`Lock`]. This ensures that no sweep can be performed
/// if a [`Ref`] pointer is around.
pub struct Ref<'p, T>
where
    T: ?Sized,
{
    p: ptr::NonNull<T>,
    _m: PhantomData<&'p GcPool>,
}

impl<'p, T> Clone for Ref<'p, T>
where
    T: ?Sized,
{
    fn clone(&self) -> Self {
        Self {
            p: self.p,
            _m: self._m,
        }
    }
}
impl<'p, T> Copy for Ref<'p, T> where T: ?Sized {}

pub type ObjRef<'p> = Ref<'p, dyn Obj>;

impl<'p, T> std::fmt::Debug for Ref<'p, T>
where
    T: Obj + ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{:?}",self.addr(), self.as_ref())
    }
}

/// Describes an object that can be garbage collected
pub trait Gc {
    /// Set pointer to next object in the pool
    fn set_next_obj(&mut self, p: Option<ObjPointer>);
    /// Return pointer to next object in the pool
    fn next_obj(&self) -> Option<ObjPointer>;
    /// Mark this object and its children as reachable
    fn mark(&mut self);
    /// Cancel mark on this object, but NOT on its children
    fn unmark(&mut self);
    /// If the object is marked as reachable
    fn is_marked(&self) -> bool;
}

/// Describes an object in the VM
pub trait Obj: Gc + std::fmt::Debug {
    fn kind(&self) -> ObjectKind;
}

/// Used for dynamic checked cast
pub trait StaticObjKind {
    fn kind() -> ObjectKind;
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ObjectKind {
    Array,
    List,
    Callable,
    String,
}

pub struct Lock {
    private: PhantomData<()>,
}

impl Lock {
    fn new() -> Self {
        Lock {
            private: PhantomData,
        }
    }
}

/// # Safety
/// All pointers must be valid
pub struct GcPool {
    first: Option<ObjPointer>,
    /// In bytes
    memory_usage: usize,
}

impl std::fmt::Debug for GcPool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug_list = f.debug_list();
        let mut p = self.first;
        while let Some(obj) = p {
            unsafe {
                debug_list.entry(&obj.to_ref(PhantomData));
                p = obj.next_obj();
            }
        }
        debug_list.finish()
    }
}

static mut GC_POOL_CNT: usize = 0;

impl GcPool {
    /// # Safety
    /// `obj` must point to valid object.
    unsafe fn push(&mut self, mut obj: ObjPointer) {
        if let Some(first) = self.first {
            obj.set_next_obj(Some(first));
        }
        self.first = Some(obj);
    }
    unsafe fn push_and_cast<'p, T>(&mut self, ptr: Pointer<T>) -> Ref<'p, T>
    where
        T: Obj + 'static,
    {
        unsafe {
            self.push(ptr.clone().cast_obj_pointer());
        }
        self.memory_usage += std::mem::size_of::<T>();
        Ref {
            p: ptr.0,
            _m: PhantomData,
        }
    }
    pub fn allocate_array<'p>(
        &mut self,
        tag: u32,
        _lock: &'p Lock,
    ) -> Result<Ref<'p, ObjArray>, ()> {
        let ptr = ObjArray::allocate(tag)?;
        // A newly create pointer is of course valid
        Ok(unsafe { self.push_and_cast(ptr) })
    }
    pub fn allocate_list<'p>(&mut self, _lock: &'p Lock) -> Result<Ref<'p, ObjList>, ()> {
        let ptr = ObjList::allocate()?;
        Ok(unsafe { self.push_and_cast(ptr) })
    }
    pub fn allocate_callable<'p>(
        &mut self,
        f: Routine,
        arity: usize,
        _lock: &'p Lock,
    ) -> Result<Ref<'p, ObjCallable>, ()> {
        let ptr = ObjCallable::allocate(f, arity)?;
        unsafe {
            self.push(ptr.clone().cast_obj_pointer());
            self.push(ptr.env().cast_obj_pointer());
        }
        self.memory_usage += std::mem::size_of::<ObjCallable>();
        Ok(Ref {
            p: ptr.0,
            _m: PhantomData,
        })
    }
    pub fn allocate_callable_with_env<'p>(
        &mut self,
        f: Routine,
        arity: usize,
        env: Pointer<ObjList>,
        env_len: usize,
        _lock: &'p Lock,
    ) -> Result<Ref<'p, ObjCallable>, ()> {
        let ptr = ObjCallable::allocate_with_env(f, arity, env, env_len)?;
        unsafe {
            self.push(ptr.clone().cast_obj_pointer());
        }
        self.memory_usage += std::mem::size_of::<ObjCallable>();
        Ok(Ref {
            p: ptr.0,
            _m: PhantomData,
        })
    }
    pub fn allocate_string<'p>(
        &mut self,
        s: &str,
        _lock: &'p Lock,
    ) -> Result<Ref<'p, ObjString>, ()> {
        let ptr = ObjString::from_str(s)?;
        Ok(unsafe { self.push_and_cast(ptr) })
    }
    pub fn allocate_string_empty<'p>(&mut self, _lock: &'p Lock) -> Result<Ref<'p, ObjString>, ()> {
        let ptr = ObjString::allocate()?;
        Ok(unsafe { self.push_and_cast(ptr) })
    }
    /// Remove unmarked objects. a [`ValueVecMarked`] must be consumed as objects are no longer marked
    /// after this function is called.
    pub fn sweep<'p>(&'p mut self, vv: &mut ValueVec, _lock: &'p mut Lock) {
        // All pointer here should be valid

        #[cfg(feature = "debug_gc")]
        println!("--- GC SWEEP BEGIN ---");

        unsafe {
            vv.mark();
            while let Some(mut first) = self.first {
                if !first.is_marked() {
                    let new_first = first.next_obj();
                    self.memory_usage -= first.size();

                    #[cfg(feature = "debug_gc")]
                    println!("Deallocated [{:?}] {:?}", first.0, first.as_ref());

                    first.deallocate();
                    self.first = new_first;

                } else {
                    first.unmark();
                    break;
                }
            }
            if let Some(first) = self.first {
                let mut p = first;
                while let Some(mut p_next) = p.next_obj() {
                    if !p_next.is_marked() {
                        let new_next = p_next.next_obj();
                        self.memory_usage -= p_next.size();

                        #[cfg(feature = "debug_gc")]
                        println!("Deallocated [{:?}] {:?}", p_next.0, p_next.as_ref());

                        p_next.deallocate();
                        p.set_next_obj(new_next);

                    } else {
                        p_next.unmark();
                        p = p_next;
                    }
                }
            }
        }

        #[cfg(feature = "debug_gc")]
        println!("--- GC SWEEP END ---");
    }
    pub fn new() -> (Self, super::ValueVec, Lock) {
        unsafe {
            if GC_POOL_CNT == 1 {
                panic!("Only one GC pool can be instantiated");
            }
            GC_POOL_CNT += 1;
        }
        (
            Self {
                first: None,
                memory_usage: 0,
            },
            super::ValueVec::new(),
            Lock::new(),
        )
    }
    pub fn memory_usage(&self) -> usize {
        self.memory_usage
    }
    pub fn pack<'p>(self, value: ValueSafe<'p>) -> MemPack {
        MemPack {
            pool: self,
            value: Value::from_safe(value),
        }
    }
    pub fn clear<'p>(&mut self, _lock: &'p mut Lock) {
        #[cfg(feature = "debug_gc")]
        {
            println!("--- GC CLEAR ---");
            println!("{:#?}", self);
        }

        while let Some(obj) = self.first {
            unsafe {
                self.first = obj.next_obj();
                obj.deallocate();
            }
        }
    }
}

pub struct MemPack {
    pool: GcPool,
    value: Value,
}

impl MemPack {
    pub fn unpack<'p>(self, _lock: &'p Lock) -> (GcPool, ValueSafe<'p>) {
        (self.pool, unsafe { self.value.to_safe(PhantomData) })
    }
}
