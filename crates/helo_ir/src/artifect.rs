use helo_runtime::builtins as rt_builtins;
use helo_runtime::{byte_code, executable};

use crate::{ir, lir, lir::ssa};
use crate::{lower_ast, lower_ir, lower_lir};

use helo_parse::builtins;
use helo_parse::errors;
use helo_parse::{ast, typed};
use helo_parse::{infer, parse};

use std::sync::Arc;

pub fn parse<'s>(
    src_str: &'s str,
    src: Arc<String>,
    file_name: Arc<String>,
) -> miette::Result<(ast::Symbols<'s>, ast::ExprHeap<'s>)> {
    let mut symbbols = ast::Symbols::new();
    let mut ast_nodes = ast::ExprHeap::new();
    let mut e = errors::ManyError::new();
    let mut ptable = parse::PrecedenceTable::new();

    builtins::add_builtins_to(&mut symbbols, &mut ptable);

    let mut tast_symbols = parse::tast::Symbols::new();
    parse::parse_ast(
        src_str,
        src.clone(),
        file_name,
        &mut tast_symbols,
        &mut ptable,
    )?;
    parse::lower_symbols(tast_symbols, &mut symbbols, &mut ast_nodes, &mut e);
    e.emit()?;

    Ok((symbbols, ast_nodes))
}

pub fn infer_type<'s>(
    symbols: ast::Symbols<'s>,
    ast_nodes: ast::ExprHeap<'s>,
) -> miette::Result<(typed::Symbols<'s>, typed::ExprHeap<'s>)> {
    let mut typed_nodes = typed::ExprHeap::new();
    let mut typed_functions = typed::FunctionTable::new();
    let mut e = errors::ManyError::new();

    for f_name in symbols.function_names() {
        let f = infer::infer_function(
            &f_name,
            &symbols,
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

    let symbols = typed::Symbols::new(symbols, typed_functions);
    Ok((symbols, typed_nodes))
}

pub fn compiler_ir<'s>(
    symbols: typed::Symbols<'s>,
    typed_nodes: typed::ExprHeap<'s>,
) -> (ir::FunctionTable, ir::ExprHeap<'s>, ir::StrList) {
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
    (ir_functions, ir_nodes, str_list)
}

pub fn compile_lir<'s>(
    ir_functions: ir::FunctionTable,
    ir_nodes: ir::ExprHeap<'s>,
) -> lir::FunctionTable<lir::Function> {
    let mut lir_functions = lir::FunctionTable::new();
    let rt_builtins = rt_builtins::BuiltinTable::new();
    for fid in ir_functions.function_names() {
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
    lir::optimizations::control_flow_simplification(&mut f.blocks, f.body, &mut f.block_run);
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

pub fn optimize_lir<'s>(
    lir_functions: lir::FunctionList<lir::Function>,
) -> lir::FunctionList<lir::FunctionOptimized> {
    lir_functions
        .into_iter()
        .map(|f| compile_ssa(f))
        .map(|(f, dom_tree)| (constant_propagation(f), dom_tree))
        .map(|(f, dom_tree)| (dead_code_elimination(f), dom_tree))
        .map(|(f, dom_tree)| common_expression_elimination(f, &dom_tree))
        .map(|f| deconstruct_ssa(f))
        .map(|f| control_flow_simplification(f))
        .collect()
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
