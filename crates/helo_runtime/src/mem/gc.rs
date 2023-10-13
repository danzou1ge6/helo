use crate::{builtins, byte_code};

use super::{value, ValueVec};
use value::{Value, ValueSafe};

use core::ptr;
use std::{alloc, marker::PhantomData};

pub struct Pointer<T>(ptr::NonNull<T>)
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
        };
        alloc::dealloc(self.0.as_ptr() as *mut u8, layout);
    }
    pub unsafe fn size(self) -> usize {
        match self.kind() {
            ObjectKind::Array => std::mem::size_of::<ObjArray>(),
            ObjectKind::List => std::mem::size_of::<ObjList>(),
            ObjectKind::Callable => std::mem::size_of::<ObjCallable>(),
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
        self.0.as_mut().mark()
    }
    pub unsafe fn unmark(&mut self) {
        self.0.as_mut().unmark()
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
        write!(f, "Ref({:?})", self.p)
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
pub trait Obj: Gc {
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
}

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
        self.gc_marker = true;
        self.v.iter_mut().for_each(|v| unsafe { v.mark() });
    }
    fn unmark(&mut self) {
        self.gc_marker = false;
    }
    fn is_marked(&self) -> bool {
        self.gc_marker
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
            *obj_arr = ObjArray {
                tag,
                v: Vec::new(),
                gc_marker: false,
                next_obj: None,
            };

            let trait_ptr = ptr::NonNull::new_unchecked(ptr);
            Ok(trait_ptr.into())
        }
    }
}

impl Pointer<ObjArray> {
    pub unsafe fn tag(&self) -> u32 {
        self.0.as_ref().tag
    }
    pub unsafe fn push(&mut self, v: Value) {
        self.0.as_mut().v.push(v)
    }
    pub unsafe fn get(&self, idx: usize) -> Option<Value> {
        self.0.as_ref().v.get(idx).copied()
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

/// A list with each node garbage collected
/// # Safety
/// All pointers in this struct must be valid
pub struct ObjList {
    v: Option<(Value, Pointer<ObjList>)>,
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
        self.gc_marker = true;
        if let Some((v, next)) = &mut self.v {
            // Here we believe that `is_obj` ensures `v` is an object pointer;
            // And `ObjList` itself ensures `v` points to a valid object
            unsafe { v.mark() }
            // Here `ObjList` itself ensures `next` points to a valid object
            unsafe { next.mark() }
        }
    }
    fn unmark(&mut self) {
        self.gc_marker = false;
    }
    fn is_marked(&self) -> bool {
        self.gc_marker
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

            let obj_arr = &mut *ptr;
            *obj_arr = ObjList {
                v: None,
                gc_marker: false,
                next_obj: None,
            };

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
    pub unsafe fn con<'p>(self, v: Value) -> Result<Pointer<ObjList>, ()> {
        let mut head = ObjList::allocate()?;
        head.0.as_mut().v = Some((v, self));
        Ok(head)
    }
    pub unsafe fn head<'p>(self) -> Option<Value> {
        self.0.as_ref().v.map(|(v, _)| v)
    }
    pub unsafe fn tail<'p>(self) -> Option<Pointer<ObjList>> {
        self.0.as_ref().v.map(|(_, list)| list)
    }
}

impl<'p> Ref<'p, ObjList> {
    pub fn con(self, v: ValueSafe<'p>) -> Result<Ref<'p, ObjList>, ()> {
        unsafe {
            Pointer::from_ref(self)
                .con(Value::from_safe(v))
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
    pub fn allocate_with_env(
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
    pub fn allocate(f: Routine, arity: usize) -> Result<Pointer<ObjCallable>, ()> {
        let env = ObjList::allocate()?;
        Ok(ObjCallable::allocate_with_env(f, arity, env)?)
    }
}

impl Pointer<ObjCallable> {
    pub unsafe fn arity(self) -> usize {
        self.0.as_ref().arity
    }
    pub unsafe fn routine(self) -> Routine {
        self.0.as_ref().f
    }
    pub unsafe fn env(self) -> Pointer<ObjList> {
        self.0.as_ref().env
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
    ) -> Result<Ref<'p, ObjCallable>, ()> {
        let mut new_env = self.env();
        for value in values {
            new_env = new_env.con(value)?;
        }
        let new_env = Pointer::from_ref(new_env);
        let callable = ObjCallable::allocate_with_env(self.routine(), self.arity(), new_env)?;
        unsafe { Ok(callable.to_ref(PhantomData)) }
    }
    pub fn env_iter(&self) -> ListRefIter<'p> {
        self.env().iter()
    }
    pub fn env_len(&self) -> usize {
        unsafe { self.p.as_ref().env_len }
    }
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

impl GcPool {
    /// # Safety
    /// `obj` must point to valid object.
    unsafe fn push(&mut self, mut obj: ObjPointer) {
        if let Some(first) = self.first {
            obj.set_next_obj(Some(first));
        }
        self.first = Some(obj);
    }
    pub fn allocate_array<'p>(
        &mut self,
        tag: u32,
        _lock: &'p Lock,
    ) -> Result<Ref<'p, ObjArray>, ()> {
        let ptr = ObjArray::allocate(tag)?;
        // A newly create pointer is of course valid
        unsafe {
            self.push(ptr.clone().cast_obj_pointer());
        }
        self.memory_usage += std::mem::size_of::<ObjArray>();
        Ok(Ref {
            p: ptr.0,
            _m: PhantomData,
        })
    }
    pub fn allocate_list<'p>(&mut self, _lock: &'p Lock) -> Result<Ref<'p, ObjList>, ()> {
        let ptr = ObjList::allocate()?;
        unsafe {
            self.push(ptr.clone().cast_obj_pointer());
        }
        self.memory_usage += std::mem::size_of::<ObjList>();
        Ok(Ref {
            p: ptr.0,
            _m: PhantomData,
        })
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
        }
        self.memory_usage += std::mem::size_of::<ObjCallable>();
        Ok(Ref {
            p: ptr.0,
            _m: PhantomData,
        })
    }
    /// Remove unmarked objects. a [`ValueVecMarked`] must be consumed as objects are no longer marked
    /// after this function is called.
    pub fn sweep<'p>(&'p mut self, vv: &mut ValueVec, _lock: &'p mut Lock) {
        // All pointer here should be valid
        unsafe {
            vv.mark();
            while let Some(mut first) = self.first {
                if !first.is_marked() {
                    let new_first = first.next_obj();
                    self.memory_usage -= first.size();
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
                        p_next.deallocate();
                        p.set_next_obj(new_next);
                    } else {
                        p_next.unmark();
                        p = p_next;
                    }
                }
            }
        }
    }
    pub fn new() -> (Self, super::ValueVec, Lock) {
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
}

impl Drop for GcPool {
    fn drop(&mut self) {
        while let Some(obj) = self.first {
            unsafe {
                self.first = obj.next_obj();
                obj.deallocate();
            }
        }
    }
}
