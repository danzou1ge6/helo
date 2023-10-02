use crate::byte_code;

pub struct Builtins {}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BuiltinId(pub(crate) usize);

impl BuiltinId {
    pub fn byte_code(self) -> byte_code::BuiltinId {
        if self.0 <= u16::MAX as usize {
            byte_code::BuiltinId::from(self.0 as u16)
        } else {
            panic!("BuiltinId out of range of 2 bytes")
        }
    }
}

impl Builtins {
    pub fn id_by_name(&self, name: &str) -> BuiltinId {
        unimplemented!()
    }
    pub fn arity_by_name(&self, name: &str) -> usize {
        unimplemented!()
    }
}
