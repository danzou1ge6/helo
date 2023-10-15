#![allow(non_upper_case_globals, non_camel_case_types)]
#![feature(split_array)]
#![feature(generic_const_exprs)]
#![feature(generic_arg_infer)]
#![feature(round_char_boundary)]

pub mod builtins;
pub mod byte_code;
pub mod disassembler;
pub mod errors;
pub mod executable;
pub mod mem;
pub mod vm;
