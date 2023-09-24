pub struct Builtins {}

pub struct BuiltinId(pub(crate) usize);

impl Builtins {
    pub fn get_by_name(&self, name: &str) -> BuiltinId {
        unimplemented!()
    }
}
