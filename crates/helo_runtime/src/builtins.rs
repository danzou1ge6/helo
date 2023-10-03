use std::collections::HashMap;

pub struct Builtins {
    tab: HashMap<&'static str, u16>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BuiltinId(pub(crate) u16);

pub struct Builtin {
    arity: usize,
}

const BUILTINS: [(&'static str, Builtin); 5] = [
    ("+", Builtin { arity: 2 }),
    ("-", Builtin { arity: 2 }),
    ("*", Builtin { arity: 2 }),
    ("/", Builtin { arity: 2 }),
    ("==", Builtin { arity: 2 }),
];

impl Builtins {
    pub fn new() -> Self {
        let mut tab = HashMap::new();
        for (i, (name, _)) in BUILTINS.iter().enumerate() {
            tab.insert(*name, u16::try_from(i).unwrap());
        }
        Self { tab }
    }
    pub fn name_by_id(id: BuiltinId) -> &'static str {
        BUILTINS[id.0 as usize].0
    }
    pub fn id_by_name(&self, name: &str) -> BuiltinId {
        BuiltinId(*self.tab.get(name).unwrap())
    }
    pub fn arity_by_name(&self, name: &str) -> usize {
        let idx = self.tab.get(name).unwrap();
        BUILTINS[*idx as usize].1.arity
    }
}
