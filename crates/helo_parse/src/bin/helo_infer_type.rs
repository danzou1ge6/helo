use helo_parse::ast;
use helo_parse::builtins;
use helo_parse::errors;
use helo_parse::infer;
use helo_parse::inferer;
use helo_parse::parse;
use helo_parse::typed;
use miette::Context;
use miette::IntoDiagnostic;

use std::io::Read;
use std::sync::Arc;

fn type_src(src: String, file_name: String) -> miette::Result<()> {
    let src = Arc::new(src);
    let file_name = Arc::new(file_name);
    let s = src.clone();

    let mut symbols = ast::Symbols::new();
    let mut ast_ndoes = ast::ExprHeap::new();
    let mut e = errors::ManyError::new();
    let mut ptable = parse::PrecedenceTable::new();

    builtins::add_builtins_to(&mut symbols, &mut ast_ndoes, &mut ptable);

    parse::parse_ast(
        &s[..],
        src.clone(),
        file_name,
        &mut symbols,
        &mut ast_ndoes,
        &mut e,
        &mut ptable,
    )?;
    e.emit()?;
    let mut e = errors::ManyError::new();

    let mut typed_nodes = typed::ExprHeap::new();
    let mut typed_functions = typed::FunctionTable::new();

    for f_name in symbols.function_names() {
        let f = infer::infer_function(
            &f_name,
            &symbols,
            &ast_ndoes,
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
    let args: Vec<_> = env::args().collect();
    let file_name = args
        .get(1)
        .unwrap_or_else(|| panic!("Usage: helo_infer_type <file_name>"))
        .clone();
    // let file_name = "helo_scripts/guard.helo".to_string();
    let mut file = fs::File::open(&file_name)
        .into_diagnostic()
        .wrap_err("Open source file failed")?;
    let mut src = String::new();
    file.read_to_string(&mut src)
        .into_diagnostic()
        .wrap_err("Read source file failed")?;
    type_src(src, file_name)
}
