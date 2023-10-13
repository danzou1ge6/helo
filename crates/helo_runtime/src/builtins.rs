use crate::{mem, vm};
use mem::ValueSafe;
use std::collections::HashMap;
use vm::Vm;

pub struct BuiltinTable {
    tab: HashMap<&'static str, BuiltinId>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct BuiltinId(pub(crate) u16);

type BuiltinFunc<const N: usize> = for<'p> fn([ValueSafe<'p>; N]) -> ValueSafe<'p>;

fn add<'p>([arg0, arg1]: [ValueSafe<'p>; 2]) -> ValueSafe<'p> {
    match (arg0, arg1) {
        (ValueSafe::Int(a), ValueSafe::Int(b)) => ValueSafe::Int(a + b),
        _ => panic!("Type Error: Integer-adding {:?} and {:?}", arg0, arg1),
    }
}

#[derive(Clone, Copy)]
pub enum Builtin {
    B1(BuiltinFunc<1>),
    B2(BuiltinFunc<2>),
    B3(BuiltinFunc<3>),
    B4(BuiltinFunc<4>),
}
use Builtin::*;

impl Builtin {
    pub fn arity(&self) -> usize {
        match self {
            B1(..) => 1,
            B2(..) => 2,
            B3(..) => 3,
            B4(..) => 4,
        }
    }
    pub fn unwrap1(self) -> BuiltinFunc<1> {
        match self {
            B1(f) => f,
            _ => panic!(
                "Type Error: Expected a builtin with 1 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap2(self) -> BuiltinFunc<2> {
        match self {
            B2(f) => f,
            _ => panic!(
                "Type Error: Expected a builtin with 2 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap3(self) -> BuiltinFunc<3> {
        match self {
            B3(f) => f,
            _ => panic!(
                "Type Error: Expected a builtin with 3 arguments, got {}",
                self.arity()
            ),
        }
    }
    pub fn unwrap4(self) -> BuiltinFunc<4> {
        match self {
            B4(f) => f,
            _ => panic!(
                "Type Error: Expected a builtin with 4 arguments, got {}",
                self.arity()
            ),
        }
    }
}

const BUILTINS: [(&'static str, Builtin); 5] = [
    ("+", B2(add)),
    ("-", B2(add)),
    ("*", B2(add)),
    ("/", B2(add)),
    ("==", B2(add)),
];

pub fn name_by_id(id: BuiltinId) -> &'static str {
    BUILTINS[id.0 as usize].0
}

pub fn get(id: BuiltinId) -> Builtin {
    BUILTINS[id.0 as usize].1
}

pub fn get_arity(id: BuiltinId) -> usize {
    get(id).arity()
}

pub fn call_adapted<'p>(id: BuiltinId, args: Vec<ValueSafe<'p>>) -> ValueSafe<'p> {
    let builtin = get(id);
    match builtin {
        B1(f) => f([args[0]]),
        B2(f) => f([args[0], args[1]]),
        B3(f) => f([args[0], args[1], args[2]]),
        B4(f) => f([args[0], args[1], args[2], args[3]]),
    }
}

impl BuiltinTable {
    pub fn new() -> Self {
        let mut tab = HashMap::new();
        for (i, (name, _)) in BUILTINS.iter().enumerate() {
            tab.insert(*name, BuiltinId(i as u16));
        }
        Self { tab }
    }
    pub fn id_by_name(&self, name: &str) -> BuiltinId {
        *self.tab.get(name).unwrap()
    }
    pub fn arity_by_name(&self, name: &str) -> usize {
        let id = self.tab.get(name).unwrap();
        get_arity(*id)
    }
}
