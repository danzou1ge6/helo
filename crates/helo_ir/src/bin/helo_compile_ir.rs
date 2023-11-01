use std::env;
use std::fs;
use std::io::Read;
use std::sync::Arc;

use miette::Context;
use miette::IntoDiagnostic;
fn compile(src: String, file_name: String) -> miette::Result<()> {
    use helo_ir::artifect;
    use helo_ir::pretty_print;

    let src = Arc::new(src);
    let file_name = Arc::new(file_name);
    let s = &src.clone()[..];

    let (ast_symbols, ast_nodes) = artifect::parse(s, src, file_name)?;
    let (typed_symbols, typed_nodes) = artifect::infer_type(ast_symbols, ast_nodes)?;
    let (ir_functions, ir_nodes, str_list) = artifect::compiler_ir(typed_symbols, typed_nodes);

    for f_name in ir_functions.function_names() {
        #[cfg(feature = "term_width")]
        let terminal_size = {
            use terminal_size;
            terminal_size.terminal_size().unwrap().0 .0
        };
        #[cfg(not(feature = "term_width"))]
        let term_width = 80;
        let allocator = pretty::RcAllocator;
        let doc_builder = pretty_print::pretty_ir_function::<_, ()>(
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
    let file_name = "helo_scripts/calculator.helo".to_string();
    let mut file = fs::File::open(&file_name)
        .into_diagnostic()
        .wrap_err("Open source file failed")?;
    let mut src = String::new();
    file.read_to_string(&mut src)
        .into_diagnostic()
        .wrap_err("Read source file failed")?;
    compile(src, file_name)
}
