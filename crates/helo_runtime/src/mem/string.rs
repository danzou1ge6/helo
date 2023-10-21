use super::objects::{Gc, Obj, ObjPointer, ObjectKind, Pointer, Ref, StaticObjKind};
use super::{GcPool, Lock};

use core::ptr;
use std::alloc;
use std::marker::PhantomData;

pub struct ObjString {
    v: String,
    next: Option<Pointer<ObjString>>,
    gc_marker: bool,
    next_obj: Option<ObjPointer>,
}

impl Gc for ObjString {
    fn set_next_obj(&mut self, p: Option<ObjPointer>) {
        self.next_obj = p
    }
    fn next_obj(&self) -> Option<ObjPointer> {
        self.next_obj
    }
    fn mark(&mut self) {
        if !self.gc_marker {
            self.gc_marker = true;
            if let Some(next) = &mut self.next {
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

impl StaticObjKind for ObjString {
    fn kind() -> ObjectKind {
        ObjectKind::String
    }
}

impl std::fmt::Debug for ObjString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"")?;
        f.write_str(&self.v)?;
        if let Some(next) = self.next {
            unsafe { next.as_ref().fmt(f) }?;
        } else {
            Ok(())?;
        }
        write!(f, "\"")
    }
}

impl Obj for ObjString {
    fn kind(&self) -> ObjectKind {
        ObjectKind::String
    }
}

impl ObjString {
    const CHUNK_SIZE: usize = 1024;

    pub(super) fn allocate() -> Result<Pointer<ObjString>, ()> {
        unsafe {
            let layout = alloc::Layout::new::<ObjString>();
            let ptr = alloc::alloc(layout) as *mut ObjString;
            if ptr.is_null() {
                return Err(());
            }

            let obj_str = &mut *ptr;
            ptr::write(ptr::addr_of!(obj_str.v) as *mut _, String::new());
            obj_str.next = None;
            obj_str.gc_marker = false;
            obj_str.next_obj = None;

            let trait_ptr = ptr::NonNull::new(ptr).unwrap();
            Ok(trait_ptr.into())
        }
    }
    pub(super) fn from_str(s: &str) -> Result<Pointer<ObjString>, ()> {
        let mut p = ObjString::allocate()?;
        let mut s = s;

        while s.len() != 0 {
            let first_chunk_len = s.floor_char_boundary(ObjString::CHUNK_SIZE);
            unsafe {
                p.as_mut().v = s[..first_chunk_len].to_string();
            }
            s = &s[first_chunk_len..];
        }

        Ok(p)
    }
}

impl Pointer<ObjString> {
    pub unsafe fn chunk_len(self) -> usize {
        self.as_ref().v.len()
    }
    pub unsafe fn rear_chunk(self) -> Pointer<ObjString> {
        if let Some(next) = self.as_ref().next {
            next.rear_chunk()
        } else {
            self
        }
    }
    pub unsafe fn clone_chunk(
        self,
        pool: &mut GcPool,
        lock: &Lock,
    ) -> Result<Pointer<ObjString>, ()> {
        let mut new_chunk = Pointer::from_ref(pool.allocate_string_empty(lock)?);
        new_chunk.as_mut().v = self.as_ref().v.clone();
        new_chunk.as_mut().next = self.as_ref().next;
        Ok(new_chunk)
    }
    pub unsafe fn concat(
        self,
        rhs: Pointer<ObjString>,
        pool: &mut GcPool,
        lock: &Lock,
    ) -> Result<Pointer<ObjString>, ()> {
        unsafe fn concat(
            lhs: Option<Pointer<ObjString>>,
            rhs: Pointer<ObjString>,
        ) -> Result<Pointer<ObjString>, ()> {
            if let Some(lhs) = lhs {
                let mut new_rhs = concat(lhs.as_ref().next, rhs)?;

                let rhs_head_sapce_left = ObjString::CHUNK_SIZE - new_rhs.chunk_len();
                if lhs.as_ref().v.len() != 0 {
                    let lhs_chunk_str = &lhs.as_ref().v;
                    // Copy to new_rhs until the chunk is filled
                    let rhs_head_inserted_cnt =
                        lhs_chunk_str.ceil_char_boundary(rhs_head_sapce_left);
                    new_rhs
                        .as_mut()
                        .v
                        .insert_str(0, &lhs_chunk_str[rhs_head_inserted_cnt..]);

                    // Rest are copied to a new chunk
                    let mut new_chunk = ObjString::allocate()?;
                    new_chunk.as_mut().v = lhs_chunk_str[..rhs_head_inserted_cnt].to_string();
                    new_chunk.as_mut().next = Some(new_rhs);

                    Ok(new_chunk)
                } else {
                    Ok(new_rhs)
                }
            } else {
                Ok(rhs)
            }
        }
        concat(Some(self), rhs.clone_chunk(pool, lock)?)
    }
    pub unsafe fn head(self) -> Option<char> {
        self.as_ref().v.chars().next()
    }
    pub unsafe fn tail(self, pool: &mut GcPool, lock: &Lock) -> Option<Pointer<ObjString>> {
        let mut new_head = Pointer::from_ref(pool.allocate_string_empty(lock).unwrap());
        if self.as_ref().v.len() != 0 {
            new_head.as_mut().v = {
                let begin = self.as_ref().v.chars().next().unwrap().len_utf8();
                self.as_ref().v[begin..].to_string()
            };
            new_head.as_mut().next = self.as_ref().next;
            Some(new_head)
        } else {
            None
        }
    }
    pub unsafe fn len(self) -> usize {
        let mut len = 0;
        let mut p = Some(self);
        while let Some(chunk) = p {
            len += chunk.chunk_len();
            p = chunk.as_ref().next;
        }
        len
    }
    pub unsafe fn eq(self, rhs: Self) -> bool {
        let mut p1 = Some(self);
        let mut p2 = Some(rhs);

        loop {
            match (p1, p2) {
                (Some(s1), Some(s2)) => {
                    if s1.as_ref().v != s2.as_ref().v {
                        return false;
                    }
                    p1 = s1.as_ref().next;
                    p2 = s2.as_ref().next;
                }
                (None, None) => break,
                _ => return false,
            }
        }
        true
    }
    pub unsafe fn eq_str(self, rhs: &str) -> bool {
        let mut p = Some(self);
        let mut s = rhs;

        loop {
            match (p, s) {
                (None, "") => return true,
                (Some(s1), s2) => {
                    let len = s1.chunk_len();
                    if s2.len() < len {
                        return false;
                    }
                    if s1.as_ref().v != s2[..len] {
                        return false;
                    }
                    p = s1.as_ref().next;
                    s = &s2[len..];
                }
                _ => return false,
            }
        }
    }
}

impl<'p> Ref<'p, ObjString> {
    pub fn concat(
        self,
        rhs: Ref<'p, ObjString>,
        pool: &mut GcPool,
        lock: &Lock,
    ) -> Result<Ref<'p, ObjString>, ()> {
        unsafe {
            Pointer::from_ref(self)
                .concat(Pointer::from_ref(rhs), pool, lock)
                .map(|p| p.to_ref(PhantomData))
        }
    }
    pub fn head(self) -> Option<char> {
        unsafe { Pointer::from_ref(self).head() }
    }
    pub fn tail(self, pool: &mut GcPool, lock: &Lock) -> Option<Ref<'p, ObjString>> {
        unsafe {
            Pointer::from_ref(self)
                .tail(pool, lock)
                .map(|p| p.to_ref(PhantomData))
        }
    }
    pub fn len(self) -> usize {
        unsafe { Pointer::from_ref(self).len() }
    }
}

impl<'p> PartialEq for Ref<'p, ObjString> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { Pointer::from_ref(*self).eq(Pointer::from_ref(*other)) }
    }
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}
impl<'p> Eq for Ref<'p, ObjString> {}

impl<'p> PartialEq<str> for Ref<'p, ObjString> {
    fn eq(&self, other: &str) -> bool {
        unsafe { Pointer::from_ref(*self).eq_str(other) }
    }
    fn ne(&self, other: &str) -> bool {
        !self.eq(other)
    }
}
