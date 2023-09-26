pub struct Builtins {}

#[derive(Debug, Clone, Copy)]
pub struct BuiltinId(pub(crate) usize);

impl Builtins {
    pub fn id_by_name(&self, name: &str) -> BuiltinId {
        unimplemented!()
    }
    pub fn arity_by_name(&self, name: &str) -> usize {
        unimplemented!()
    }
}
