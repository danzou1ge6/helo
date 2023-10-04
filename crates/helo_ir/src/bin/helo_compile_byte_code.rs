use helo_ir::artifect;
use helo_ir::pretty_print;
use helo_runtime::disassembler;

use miette::{Context, IntoDiagnostic};

use std::env;
use std::fs;
use std::io::Read;
use std::sync::Arc;

fn pretty_lir_functions(
    lir_functions: &helo_ir::lir::FunctionList,
    str_list: &helo_ir::ir::StrList,
) {
    for lir_fid in lir_functions.iter_id() {
        let term_width = 80;
        let allocator = pretty::RcAllocator;
        let doc_builder = pretty_print::pretty_lir_function::<_, ()>(
            lir_fid,
            &str_list,
            &lir_functions,
            &allocator,
        );
        let doc = doc_builder.pretty(term_width);
        println!("{doc}");
    }
}

fn compile(src: String, file_name: String) -> miette::Result<()> {
    let src = Arc::new(src);
    let file_name = Arc::new(file_name);
    let s = &src.clone()[..];

    let (ast_symbols, ast_nodes) = artifect::parse(s, src, file_name)?;
    let (typed_symbols, typed_nodes) = artifect::infer_type(ast_symbols, ast_nodes)?;
    let (ir_functions, ir_nodes, str_list) = artifect::compiler_ir(typed_symbols, typed_nodes);

    let mut lir_functions = artifect::compile_lir(ir_functions, ir_nodes)?;
    println!("Before optimization:");
    pretty_lir_functions(&lir_functions, &str_list);
    println!("");

    // artifect::lir_optimize(&mut lir_functions);
    // println!("After optimization:");
    // pretty_lir_functions(&lir_functions, &str_list);
    // println!("");

    artifect::lir_compress_registers(&mut lir_functions);
    println!("After registers:");
    pretty_lir_functions(&lir_functions, &str_list);
    println!("");

    let executable = artifect::compile_byte_code(lir_functions, str_list)?;

    let pretty_table = disassembler::disassemble(&executable);
    println!("{pretty_table}");
    Ok(())
}

pub fn main() -> miette::Result<()> {
    // let args: Vec<_> = env::args().collect();
    // let file_name = args
    //     .get(1)
    //     .unwrap_or_else(|| panic!("Usage: helo_compile_ir <file_name>"))
    //     .clone();
    let file_name = "helo_scripts/byte_code.helo".to_string();
    let mut file = fs::File::open(&file_name)
        .into_diagnostic()
        .wrap_err("Open source file failed")?;
    let mut src = String::new();
    file.read_to_string(&mut src)
        .into_diagnostic()
        .wrap_err("Read source file failed")?;
    compile(src, file_name)
}
