use helo_runtime::{byte_code, executable};

use crate::{ir, lir, lir::ssa, pretty_print};
use crate::{lower_ast, lower_ir, lower_lir};

use helo_parse::builtins;
use helo_parse::errors;
use helo_parse::source_tree;
use helo_parse::{ast, typed};
use helo_parse::{infer, parse};

pub fn parse<'s>(
    src_tree: &'s source_tree::SourceTree,
) -> miette::Result<(ast::Symbols<'s>, ast::ExprHeap<'s>)> {
    let mut ast_nodes = ast::ExprHeap::new();
    let mut e = errors::ManyError::new();
    let mut tast_symbols = parse::tast::Symbols::new();

    builtins::add_builtins_to(&mut tast_symbols)?;
    src_tree.walk(&mut tast_symbols, &mut e);
    let mut e = e.emit()?;

    let ast_symbols = parse::lower_symbols(tast_symbols, &mut ast_nodes, &mut e);
    let _ = e.emit()?;

    Ok((ast_symbols, ast_nodes))
}

pub fn infer_type<'s>(
    symbols: ast::Symbols<'s>,
    ast_nodes: ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> (typed::Symbols<'s>, typed::ExprHeap<'s>) {
    let mut typed_nodes = typed::ExprHeap::new();
    let mut typed_functions = typed::FunctionTable::new();

    for f_id in symbols.functions.keys() {
        // Closures well be inferred in enclosing functions
        if !typed_functions.contains(&f_id) && !f_id.is_closure() {
            let f = infer::infer_function(
                f_id.clone(),
                &infer::CapturedTypeInfo::empty(),
                &symbols,
                &ast_nodes,
                &mut typed_nodes,
                &mut typed_functions,
                e,
            );
            if let Some(f) = f {
                typed_functions.insert(f_id.clone(), f);
            }
        }
    }

    let symbols = typed::Symbols::new(symbols, typed_functions);
    (symbols, typed_nodes)
}

pub fn compiler_ir<'s: 'a, 'a>(
    entries: impl Iterator<Item = &'a ast::FunctionId<'s>>,
    symbols: &typed::Symbols<'s>,
    typed_nodes: typed::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> (
    ir::FunctionTable<'s>,
    ir::ExprHeap<'s>,
    ir::StrList,
    Vec<ir::FunctionId<'s>>,
) {
    let mut ir_nodes = ir::ExprHeap::new();
    let mut str_table = ir::StrTable::new();
    let mut ir_functions = ir::FunctionTable::new();

    let ir_fids = entries
        .into_iter()
        .filter_map(|f_id| {
            let f = symbols.function(f_id);
            if f.var_cnt != 0 {
                e.push(crate::errors::InvalidEntryPoint::new(&f.meta, &f.type_));
                return None;
            }
            let inferer = helo_parse::inferer::Inferer::new();
            let ir_fid = lower_ast::lower_function(
                f_id,
                &inferer,
                &symbols,
                &typed_nodes,
                &mut ir_functions,
                &mut ir_nodes,
                &mut str_table,
                e,
            );
            Some(ir_fid)
        })
        .collect();

    let str_list = str_table.to_list();
    (ir_functions, ir_nodes, str_list, ir_fids)
}

pub use ir::optimizations::inline_ir;

pub fn compile_lir<'s>(
    ir_functions: ir::FunctionTable<'s>,
    ir_nodes: ir::ExprHeap<'s>,
) -> lir::FunctionTable<'s, lir::Function> {
    let mut lir_functions = lir::FunctionTable::new();
    let rt_builtins = ir::BuiltinTable::new();
    for fid in ir_functions.function_ids() {
        lower_ir::lower_function(
            fid,
            &ir_nodes,
            &ir_functions,
            &rt_builtins,
            &mut lir_functions,
        );
    }
    lir_functions
}

pub fn compile_ssa(f: lir::Function) -> (ssa::Function, ssa::DominanceTree) {
    let lir::Function {
        body,
        blocks,
        arity,
        meta,
        name,
        temp_cnt,
    } = f;

    let post_order = ssa::BlocksOrder::post_order_of(&blocks, body);
    let idom = ssa::ImmediateDominators::build(&blocks, body, &post_order);
    let dom_tree = ssa::DominanceTree::from_idoms(&idom);
    let dom_frontier = ssa::DominanceFrontier::build(&blocks, &idom);

    let blocks_len = blocks.len();

    let (ssa_blocks, temp_cnt) =
        ssa::construct_ssa(blocks, body, &dom_frontier, &dom_tree, temp_cnt);

    let f = ssa::Function {
        body,
        blocks: ssa_blocks,
        arity,
        meta,
        name,
        temp_cnt,
        block_run: lir::BlockIdVec::repeat(true, blocks_len),
    };

    (f, dom_tree)
}

pub fn dead_code_elimination(mut f: ssa::Function) -> ssa::Function {
    lir::optimizations::dead_code_elimination(&mut f.blocks, f.temp_cnt);
    f
}

pub fn common_expression_elimination(
    mut f: ssa::Function,
    dom_tree: &ssa::DominanceTree,
) -> ssa::Function {
    lir::optimizations::common_expression_elimination(&mut f.blocks, dom_tree, f.body, f.temp_cnt);
    f
}

pub fn constant_propagation(mut f: ssa::Function) -> ssa::Function {
    lir::optimizations::constant_propagation(&mut f.blocks, &mut f.block_run, f.body, f.temp_cnt);
    f
}

pub fn control_flow_simplification(mut f: lir::FunctionOptimized) -> lir::FunctionOptimized {
    lir::optimizations::control_flow_simplification(&mut f.blocks, &mut f.body, &mut f.block_run);
    f
}

pub fn deconstruct_ssa(f: ssa::Function) -> lir::FunctionOptimized {
    let ssa::Function {
        body,
        blocks,
        arity,
        meta,
        name,
        temp_cnt,
        block_run,
    } = f;

    let (blocks, temp_cnt) = ssa::deconstruct_ssa(blocks, temp_cnt, arity);

    let f = lir::FunctionOptimized {
        body,
        blocks,
        arity,
        meta,
        name,
        temp_cnt,
        block_run,
    };
    f
}


pub fn compile_byte_code(
    lir_functions: lir::FunctionList<lir::FunctionOptimized>,
    str_list: ir::StrList,
    main_function_id: lir::FunctionId,
) -> miette::Result<executable::Executable> {
    let mut chunk = byte_code::Chunk::new();
    let mut e = errors::ManyError::new();
    let mut f_relocations = lower_lir::FunctionRelocations::new();
    let mut functions = lower_lir::FunctionTable::new();

    let (str_index, str_chunk) = lir::StrIndex::new(str_list);
    for fid in lir_functions.iter_id() {
        lower_lir::lower_function(
            fid,
            &lir_functions,
            &str_index,
            &mut chunk,
            &mut f_relocations,
            &mut functions,
            &mut e,
        );
    }

    f_relocations.relocate(&mut chunk, &mut functions);
    let entry = functions.get(main_function_id).unwrap();
    let symbols = functions.to_symbols();

    e.emit()?;

    let executable = executable::Executable::new(chunk, entry, str_chunk, symbols);
    Ok(executable)
}

use std::io;
use std::io::Write;

pub fn format_ir_functions<'s>(
    formatter: &mut impl Write,
    ir_functions: &ir::FunctionTable<'s>,
    ir_nodes: &ir::ExprHeap<'s>,
    str_list: &ir::StrList,
    instances: &ast::InstanceTable<'s>,
    term_width: usize,
) -> io::Result<()> {
    for f_id in ir_functions.function_ids() {
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
        write!(formatter, "{doc}\n")?;
    }
    Ok(())
}

pub fn format_lir_functions(
    formatter: &mut impl Write,
    lir_functions: &lir::FunctionList<lir::Function>,
    function_names: &lir::FunctionNameList,
    str_list: &ir::StrList,
    term_width: usize,
) -> io::Result<()> {
    for lir_fid in lir_functions.iter_id() {
        let allocator = pretty::RcAllocator;
        let doc_builder = pretty_print::pretty_lir_function::<_, ()>(
            lir_fid,
            lir_functions.get(lir_fid).unwrap(),
            &str_list,
            function_names,
            &allocator,
        );
        let doc = doc_builder.pretty(term_width);
        write!(formatter, "{doc}\n")?;
    }
    Ok(())
}

pub fn format_lir_function_optimized(
    formatter: &mut impl Write,
    f: &lir::FunctionOptimized,
    lir_fid: lir::FunctionId,
    function_names: &lir::FunctionNameList,
    str_list: &ir::StrList,
    term_width: usize,
) -> io::Result<()> {
    let allocator = pretty::RcAllocator;
    let doc_builder = pretty_print::pretty_lir_function_optimized::<_, ()>(
        lir_fid,
        f,
        &str_list,
        function_names,
        &allocator,
    );
    let doc = doc_builder.pretty(term_width);
    write!(formatter, "{doc}\n")
}

pub fn format_lir_functions_optimized(
    formatter: &mut impl Write,
    lir_functions: &lir::FunctionList<lir::FunctionOptimized>,
    function_names: &lir::FunctionNameList,
    str_list: &ir::StrList,
    term_width: usize,
) -> io::Result<()> {
    for lir_fid in lir_functions.iter_id() {
        format_lir_function_optimized(
            formatter,
            &lir_functions.get(lir_fid).unwrap(),
            lir_fid,
            function_names,
            str_list,
            term_width,
        )?;
    }
    Ok(())
}

pub fn format_ssa_blocks(
    formatter: &mut impl Write,
    f_id: lir::FunctionId,
    f: &ssa::Function,
    str_list: &ir::StrList,
    function_names: &lir::FunctionNameList,
    term_width: usize,
) -> io::Result<()> {
    let allocator = pretty::RcAllocator;
    let doc_builder =
        pretty_print::pretty_ssa_function::<_, ()>(f_id, f, str_list, function_names, &allocator);
    let doc = doc_builder.pretty(term_width);
    write!(formatter, "{doc}\n")
}
