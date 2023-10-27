use helo_ir::artifect;
use helo_parse::errors;
use helo_parse::infer;
use helo_parse::typed;
use miette::Context;
use miette::IntoDiagnostic;

use std::io::Read;
use std::sync::Arc;

fn type_src(src: String, file_name: String) -> miette::Result<()> {
    let src = Arc::new(src);
    let file_name = Arc::new(file_name);
    let s = src.clone();

    let (ast_symbols, ast_nodes) = artifect::parse(&s, src, file_name)?;
    let mut e = errors::ManyError::new();

    let mut typed_nodes = typed::ExprHeap::new();
    let mut typed_functions = typed::FunctionTable::new();

    for f_name in ast_symbols.function_names() {
        let f = infer::infer_function(
            &f_name,
            &ast_symbols,
            &ast_nodes,
            &mut typed_nodes,
            &mut typed_functions,
            &mut e,
        );
        if let Some(f) = f {
            typed_functions.insert(f_name.to_string(), f);
        }
    }

    e.emit()?;

    for (name, f) in typed_functions.iter() {
        println!("{}: {}", name, f.type_);
        // typed_nodes.dbg_expr(f.body);
    }
    Ok(())
}

use std::env;
use std::fs;

fn main() -> miette::Result<()> {
    // let args: Vec<_> = env::args().collect();
    // let file_name = args
    //     .get(1)
    //     .unwrap_or_else(|| panic!("Usage: helo_infer_type <file_name>"))
    //     .clone();
    let file_name = "helo_scripts/recursion.helo".to_string();
    let mut file = fs::File::open(&file_name)
        .into_diagnostic()
        .wrap_err("Open source file failed")?;
    let mut src = String::new();
    file.read_to_string(&mut src)
        .into_diagnostic()
        .wrap_err("Read source file failed")?;
    type_src(src, file_name)
}