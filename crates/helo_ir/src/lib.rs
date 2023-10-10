#![feature(generic_arg_infer)]
pub mod artifect;
pub mod errors;
pub mod ir;
pub mod lifetime;
pub mod lir;
pub mod lower_ast;
pub mod lower_ir;
pub mod lower_lir;
#[cfg(feature = "pretty_print")]
pub mod pretty_print;
