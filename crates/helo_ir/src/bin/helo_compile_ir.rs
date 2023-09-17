use std::env;
use std::fs;
use std::io::Read;
use std::sync::Arc;

use miette::Context;
use miette::IntoDiagnostic;

fn compile(src: String, file_name: String) -> miette::Result<()> {
    use helo_parse::ast;
    use helo_parse::builtins;
    use helo_parse::errors;
    use helo_parse::infer;
    use helo_parse::parse;
    use helo_parse::typed;

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

    let symbols = typed::Symbols::new(symbols, typed_functions);

    use helo_ir::ir;
    use helo_ir::lower_ast;
    use helo_ir::pretty_print;

    let mut ir_nodes = ir::ExprHeap::new();
    let mut str_table = ir::StrTable::new();
    let mut ir_functions = ir::FunctionTable::new();

    for f_name in symbols.function_names() {
        let f = lower_ast::lower_function(
            f_name,
            &symbols,
            &typed_nodes,
            &mut ir_nodes,
            &mut str_table,
        );
        ir_functions.insert(f_name.clone(), f);
    }

    let str_list = str_table.to_list();

    for f_name in ir_functions.function_names() {
        #[cfg(feature = "term_width")]
        let terminal_size = {
            use terminal_size;
            terminal_size.terminal_size().unwrap().0 .0
        };
        #[cfg(not(feature = "term_width"))]
        let term_width = 80;
        let allocator = pretty::RcAllocator;
        let doc_builder = pretty_print::pretty_function::<_, ()>(
            ir_functions.get(f_name).unwrap(),
            &f_name,
            &ir_nodes,
            &str_list,
            &allocator,
        );
        let doc = doc_builder.pretty(term_width);
        println!("{doc}");
    }

    Ok(())
}

fn main() -> miette::Result<()> {
    // let args: Vec<_> = env::args().collect();
    // let file_name = args
    //     .get(1)
    //     .unwrap_or_else(|| panic!("Usage: helo_compile_ir <file_name>"))
    //     .clone();
    let file_name = "helo_scripts/recursion.helo".to_string();
    let mut file = fs::File::open(&file_name)
        .into_diagnostic()
        .wrap_err("Open source file failed")?;
    let mut src = String::new();
    file.read_to_string(&mut src)
        .into_diagnostic()
        .wrap_err("Read source file failed")?;
    compile(src, file_name)
}
