use helo_ir::pretty_print;
use helo_ir::{artifect, lir, lir::ssa};
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

fn print_ssa_blocks(
    f_id: lir::FunctionId,
    f_name: helo_ir::ir::StrId,
    entry: lir::BlockId,
    arity: usize,
    ssa_blocks: &ssa::SsaBlockHeap,
    str_list: &helo_ir::ir::StrList,
    lir_functions: &helo_ir::lir::FunctionList,
) {
    let term_width = 80;
    let allocator = pretty::RcAllocator;
    let doc_builder = pretty_print::pretty_ssa_function::<_, ()>(
        f_id,
        f_name,
        entry,
        ssa_blocks,
        arity,
        str_list,
        lir_functions,
        &allocator,
    );
    let doc = doc_builder.pretty(term_width);
    println!("{doc}");
}

fn compile(src: String, file_name: String) -> miette::Result<()> {
    let src = Arc::new(src);
    let file_name = Arc::new(file_name);
    let s = &src.clone()[..];

    let (ast_symbols, ast_nodes) = artifect::parse(s, src, file_name)?;
    let (typed_symbols, typed_nodes) = artifect::infer_type(ast_symbols, ast_nodes)?;
    let (ir_functions, ir_nodes, str_list) = artifect::compiler_ir(typed_symbols, typed_nodes);

    let lir_functions = artifect::compile_lir(ir_functions, ir_nodes);
    let main_fid = lir_functions.main_id();
    let mut lir_functions = lir_functions.to_list();
    println!("Before optimization:");
    pretty_lir_functions(&lir_functions, &str_list);
    println!("");

    for fid in lir_functions.iter_id() {
        let f = lir_functions.get_mut(fid).unwrap();
        let arity = f.arity;
        let f_entry = f.body;
        let f_name = f.name;
        let blocks = std::mem::take(&mut f.blocks);

        let post_order = ssa::BlocksOrder::post_order_of(&blocks, f_entry);
        let idom = ssa::ImmediateDominators::build(&blocks, f_entry, &post_order);
        let dom_tree = ssa::DominanceTree::from_idoms(&idom);
        let dom_frontier = ssa::DominanceFrontier::build(&blocks, &idom);

        let (mut ssa_blocks, temp_cnt) =
            ssa::construct_ssa(blocks, f.body, &dom_frontier, &dom_tree, f.temp_cnt);
        f.temp_cnt = temp_cnt;

        println!("Fresh SSA");
        print_ssa_blocks(
            fid,
            f_name,
            f_entry,
            arity,
            &ssa_blocks,
            &str_list,
            &lir_functions,
        );

        lir::optimizations::dead_code_elimination(&mut ssa_blocks, temp_cnt);
        println!("After dead code elimination");
        print_ssa_blocks(
            fid,
            f_name,
            f_entry,
            arity,
            &ssa_blocks,
            &str_list,
            &lir_functions,
        );

        lir::optimizations::common_expression_elimination(
            &mut ssa_blocks,
            &dom_tree,
            f_entry,
            temp_cnt,
        );
        println!("After common expression elimination");
        print_ssa_blocks(
            fid,
            f_name,
            f_entry,
            arity,
            &ssa_blocks,
            &str_list,
            &lir_functions,
        );

        let (blocks, _) = ssa::deconstruct_ssa(ssa_blocks, temp_cnt, arity);
        lir_functions.get_mut(fid).unwrap().blocks = blocks;
    }

    // artifect::lir_optimize(&mut lir_functions);
    println!("After optimization:");
    pretty_lir_functions(&lir_functions, &str_list);
    println!("");

    let executable = artifect::compile_byte_code(lir_functions, str_list, main_fid?)?;

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
