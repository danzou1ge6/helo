use helo_ir::pretty_print;
use helo_ir::{artifect, ir, lir, lir::ssa};
use helo_parse::{ast, errors, source_tree};
use helo_runtime::{disassembler, executable, vm};

use miette::{Context, IntoDiagnostic};

use std::env;
use std::fs;
use std::io::Read;

fn pretty_ir_functions<'s>(
    ir_functions: &ir::FunctionTable<'s>,
    ir_nodes: &ir::ExprHeap<'s>,
    str_list: &helo_ir::ir::StrList,
    instances: &ast::InstanceTable<'s>,
) {
    for f_id in ir_functions.function_ids() {
        #[cfg(feature = "term_width")]
        let terminal_size = {
            use terminal_size;
            terminal_size.terminal_size().unwrap().0 .0
        };
        #[cfg(not(feature = "term_width"))]
        let term_width = 80;
        let allocator = pretty::RcAllocator;
        let doc_builder = pretty_print::pretty_ir_function::<_, ()>(
            ir_functions.get(f_id).unwrap(),
            &f_id.to_string(instances),
            &ir_nodes,
            &str_list,
            instances,
            &allocator,
        );
        let doc = doc_builder.pretty(term_width);
        println!("{doc}");
    }
}

fn pretty_lir_functions(
    lir_functions: &helo_ir::lir::FunctionList<lir::Function>,
    function_names: &lir::FunctionNameList,
    str_list: &helo_ir::ir::StrList,
) {
    for lir_fid in lir_functions.iter_id() {
        let term_width = 80;
        let allocator = pretty::RcAllocator;
        let doc_builder = pretty_print::pretty_lir_function::<_, ()>(
            lir_fid,
            lir_functions.get(lir_fid).unwrap(),
            &str_list,
            function_names,
            &allocator,
        );
        let doc = doc_builder.pretty(term_width);
        println!("{doc}");
    }
}

fn print_lir_function_optimized(
    f: &lir::FunctionOptimized,
    lir_fid: lir::FunctionId,
    function_names: &helo_ir::lir::FunctionNameList,
    str_list: &helo_ir::ir::StrList,
) {
    let term_width = 80;
    let allocator = pretty::RcAllocator;
    let doc_builder = pretty_print::pretty_lir_function_optimized::<_, ()>(
        lir_fid,
        f,
        &str_list,
        function_names,
        &allocator,
    );
    let doc = doc_builder.pretty(term_width);
    println!("{doc}");
}

fn pretty_lir_functions_optimized(
    lir_functions: &helo_ir::lir::FunctionList<lir::FunctionOptimized>,
    function_names: &lir::FunctionNameList,
    str_list: &helo_ir::ir::StrList,
) {
    for lir_fid in lir_functions.iter_id() {
        print_lir_function_optimized(
            &lir_functions.get(lir_fid).unwrap(),
            lir_fid,
            function_names,
            str_list,
        )
    }
}

fn print_ssa_blocks(
    f_id: lir::FunctionId,
    f: &ssa::Function,
    str_list: &helo_ir::ir::StrList,
    function_names: &helo_ir::lir::FunctionNameList,
) {
    let term_width = 80;
    let allocator = pretty::RcAllocator;
    let doc_builder =
        pretty_print::pretty_ssa_function::<_, ()>(f_id, f, str_list, function_names, &allocator);
    let doc = doc_builder.pretty(term_width);
    println!("{doc}");
}

fn compile(file: std::path::PathBuf) -> miette::Result<executable::Executable> {
    // Parse
    let src_tree = source_tree::SourceTree::new(file)?;
    let (ast_symbols, ast_nodes) = artifect::parse(&src_tree)?;

    // Type inference and lower to IR
    let mut e = errors::ManyError::new();
    let (typed_symbols, typed_nodes) = artifect::infer_type(ast_symbols, ast_nodes, &mut e);

    println!("Infered type:");
    for (name, f) in typed_symbols.functions.iter() {
        println!("{}: {}", name.to_string(&typed_symbols.instances), f.type_);
    }

    // Check existence and type of main
    let main = ast::FunctionId::Standard(ast::Path::new(["main"]));
    if let Some(f) = typed_symbols.functions.get(&main) {
        let ast::FunctionType {
            params, captures, ..
        } = &f.type_;
        if params.len() != 0 || captures.len() != 0 {
            e.push(helo_ir::errors::MainBadType::new(&f.meta, &f.type_));
        }
    } else {
        e.push(helo_ir::errors::MainNotFound {});
    }
    let mut e = e.emit()?;

    // Lower to IR
    let (ir_functions, ir_nodes, str_list, _) =
        artifect::compiler_ir([main.clone()].iter(), &typed_symbols, typed_nodes, &mut e);
    let _ = e.emit()?;

    println!("IR:");
    pretty_ir_functions(
        &ir_functions,
        &ir_nodes,
        &str_list,
        &typed_symbols.instances,
    );

    // Lower to LIR
    let lir_functions = artifect::compile_lir(ir_functions, ir_nodes);
    let main_fid = lir_functions
        .get(&ir::FunctionId::of_non_generic(main))
        .unwrap();
    let lir_functions = lir_functions.to_list();
    let function_names = lir_functions.function_name_list();
    println!("Before optimization:");
    pretty_lir_functions(&lir_functions, &function_names, &str_list);
    println!("");

    // Optimization passes
    let lir_functions = lir_functions
        .into_iter_id()
        .map(|(fid, f)| (fid, artifect::compile_ssa(f)))
        .map(|(fid, (f, dom_tree))| {
            println!("\nFresh SSA");
            print_ssa_blocks(fid, &f, &str_list, &function_names);
            (fid, f, dom_tree)
        })
        .map(|(fid, f, dom_tree)| {
            let f = artifect::constant_propagation(f);
            println!("\nAfter constant propagation");
            print_ssa_blocks(fid, &f, &str_list, &function_names);
            (fid, f, dom_tree)
        })
        .map(|(fid, f, dom_tree)| {
            let f = artifect::dead_code_elimination(f);
            println!("\nAfter dead code elimination");
            print_ssa_blocks(fid, &f, &str_list, &function_names);
            (fid, f, dom_tree)
        })
        .map(|(fid, f, dom_tree)| {
            let f = artifect::common_expression_elimination(f, &dom_tree);
            println!("\nAfter common expression elimination");
            print_ssa_blocks(fid, &f, &str_list, &function_names);
            (fid, f)
        })
        .map(|(fid, f)| {
            let f = artifect::deconstruct_ssa(f);
            println!("\nBack to LIR");
            print_lir_function_optimized(&f, fid, &function_names, &str_list);
            (fid, f)
        })
        .map(|(_fid, f)| {
            let f = artifect::control_flow_simplification(f);
            f
        })
        .collect::<lir::FunctionList<lir::FunctionOptimized>>();

    println!("\nAfter contro flow simplification");
    pretty_lir_functions_optimized(&lir_functions, &function_names, &str_list);

    // Compile byte-code
    let executable = artifect::compile_byte_code(lir_functions, str_list, main_fid)?;

    let pretty_table = disassembler::disassemble(&executable);
    println!("\n{pretty_table}");
    Ok(executable)
}

fn run(exe: &executable::Executable) {
    let gc_policy = vm::StressedGcPolicy::new();
    let mut vm = vm::Vm::new(exe, gc_policy);
    let r = vm.run();

    match r {
        Ok((pack, mut lock)) => {
            let (mut pool, val) = pack.unpack(&lock);
            println!("Return value = {:?}", val);
            pool.clear(&mut lock);
        }
        Err(e) => match e {
            helo_runtime::errors::RunTimeError::Panic { file, span, msg } => {
                print_panic(file, span, msg);
            }
            _ => eprintln!("{:?}", e),
        },
    }
}

fn print_panic(file: String, span: (usize, usize), msg: String) {
    if let Ok(mut file) = fs::File::open(&file) {
        let mut src = String::new();
        if let Ok(_) = file.read_to_string(&mut src) {
            let report = miette::miette!(
                labels = vec![miette::LabeledSpan::at(span, msg)],
                "Program Panicked"
            )
            .with_source_code(src);
            println!("{:?}", report);
        }
    }
    println!("");
}

pub fn main() -> miette::Result<()> {
    // let args: Vec<_> = env::args().collect();
    // let file_name = args
    //     .get(1)
    //     .unwrap_or_else(|| panic!("Usage: helo_compile_ir <file_name>"))
    //     .clone();
    let file_path = std::path::PathBuf::from("helo_scripts/infix_constructor.helo");
    let mut file = fs::File::open(&file_path)
        .into_diagnostic()
        .wrap_err("Open source file failed")?;
    let mut src = String::new();
    file.read_to_string(&mut src)
        .into_diagnostic()
        .wrap_err("Read source file failed")?;
    let executable = compile(file_path)?;
    run(&executable);
    Ok(())
}
