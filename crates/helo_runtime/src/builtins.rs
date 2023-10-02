use std::collections::HashMap;

use crate::byte_code;

pub struct Builtins {
    tab: HashMap<&'static str, usize>,
}

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

pub struct Builtin {
    arity: usize,
}

const BUILTINS: [(&'static str, Builtin); 4] = [
    ("+", Builtin { arity: 2 }),
    ("-", Builtin { arity: 2 }),
    ("*", Builtin { arity: 2 }),
    ("/", Builtin { arity: 2 }),
];

impl Builtins {
    pub fn new() -> Self {
        let mut tab = HashMap::new();
        for (i, (name, _)) in BUILTINS.iter().enumerate() {
            tab.insert(*name, i);
        }
        Self { tab }
    }
    pub fn id_by_name(&self, name: &str) -> BuiltinId {
        BuiltinId(*self.tab.get(name).unwrap())
    }
    pub fn arity_by_name(&self, name: &str) -> usize {
        let idx = self.tab.get(name).unwrap();
        BUILTINS[*idx].1.arity
    }
}
