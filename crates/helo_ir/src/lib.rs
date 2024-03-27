#![feature(generic_arg_infer)]
#![feature(fmt_internals)]
pub mod artifect;
pub mod errors;
pub mod ir;
pub mod lir;
pub mod lower_ast;
pub mod lower_ir;
pub mod lower_lir;
#[cfg(feature = "pretty_print")]
pub mod pretty_print;
