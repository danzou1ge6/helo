use helo_runtime::builtins as rt_builtins;
use helo_runtime::{byte_code, executable};

use crate::{ir, lir};
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

    builtins::add_builtins_to(&mut symbbols, &mut ast_nodes, &mut ptable);

    parse::parse_ast(
        src_str,
        src.clone(),
        file_name,
        &mut symbbols,
        &mut ast_nodes,
        &mut e,
        &mut ptable,
    )?;
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
) -> miette::Result<lir::FunctionList> {
    let mut lir_functions = lir::FunctionTable::new();
    let rt_builtins = rt_builtins::Builtins::new();
    for fid in ir_functions.function_names() {
        lower_ir::lower_function(
            fid,
            &ir_nodes,
            &ir_functions,
            &rt_builtins,
            &mut lir_functions,
        );
    }
    let f_list = lir_functions.to_list()?;
    Ok(f_list)
}

pub fn compile_byte_code(
    lir_functions: lir::FunctionList,
    str_list: ir::StrList,
) -> miette::Result<executable::Executable> {
    let mut chunk = byte_code::Chunk::new();
    let mut e = errors::ManyError::new();
    let mut functions = lower_lir::FunctionTable::new();

    let (str_index, str_chunk) = lir::StrIndex::new(str_list);
    let entry = lower_lir::lower_function(
        lir_functions.main_id(),
        &lir_functions,
        &str_index,
        &mut chunk,
        &mut functions,
        &mut e,
    );

    let symbols = functions.to_symbols();

    e.emit()?;

    let executable = executable::Executable::new(chunk, entry, str_chunk, symbols);
    Ok(executable)
}
