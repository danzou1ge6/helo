use std::io;

use helo_ir::{artifect, ir, lir};
use helo_parse::{ast, errors, source_tree};
use helo_runtime::{disassembler, executable};

use std::io::Write;

use miette::IntoDiagnostic;

use clap::ValueEnum;

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
#[value(rename_all = "kebab-case")]
pub enum PrintFlag {
    InferedType,
    Ir,
    FreshLir,
    FreshSsa,
    AfterConstantPropagation,
    AfterDeadCodeElimination,
    AfterCommonExpressionElimination,
    BackToLir,
    AfterControlFlowOptimization,
    ByteCode,
}

impl PrintFlag {
    pub fn name(&self) -> &'static str {
        use PrintFlag::*;
        match self {
            InferedType => "---- Infered Type ----\n",
            Ir => "---- IR ----\n",
            FreshLir => "---- Fresh LIR ----\n",
            FreshSsa => "---- Fresh SSA ----\n",
            AfterConstantPropagation => "---- After Constant Propagation ----\n",
            AfterDeadCodeElimination => "---- After Dead Code Elimination ----\n",
            AfterCommonExpressionElimination => "---- After Common Expression Elimination ----\n",
            BackToLir => "---- Back to LIR from SSA ----\n",
            AfterControlFlowOptimization => "---- After Control Flow Optimization ----\n",
            ByteCode => "---- Byte Code ----\n",
        }
    }
}

pub struct Config {
    pub print_flags: Vec<PrintFlag>,
    pub print_all: bool,
    pub term_width: usize,
}

impl Config {
    pub fn print(&self, formatter: &mut impl Write, flag: PrintFlag) -> Result<bool, io::Error> {
        if self.print_flags.contains(&flag) || self.print_all {
            write!(formatter, "{}", flag.name())?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

pub fn compile(
    src_tree: &source_tree::SourceTree,
    config: &Config,
    formatter: &mut impl Write,
) -> miette::Result<executable::Executable> {
    // Parse
    let (ast_symbols, ast_nodes) = artifect::parse(src_tree)?;

    // Type inference and lower to IR
    let mut e = errors::ManyError::new();
    let (typed_symbols, typed_nodes) = artifect::infer_type(ast_symbols, ast_nodes, &mut e);

    if config
        .print(formatter, PrintFlag::InferedType)
        .into_diagnostic()?
    {
        for (name, f) in typed_symbols.functions.iter() {
            write!(
                formatter,
                "{}: {}",
                name.to_string(&typed_symbols.instances),
                f.type_
            )
            .into_diagnostic()?;
        }
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

    if config.print(formatter, PrintFlag::Ir).into_diagnostic()? {
        artifect::format_ir_functions(
            formatter,
            &ir_functions,
            &ir_nodes,
            &str_list,
            &typed_symbols.instances,
            config.term_width,
        )
        .into_diagnostic()?;
    }

    // Lower to LIR
    let lir_functions = artifect::compile_lir(ir_functions, ir_nodes);
    let main_fid = lir_functions
        .get(&ir::FunctionId::of_non_generic(main))
        .unwrap();
    let lir_functions = lir_functions.to_list();
    let function_names = lir_functions.function_name_list();

    if config
        .print(formatter, PrintFlag::FreshLir)
        .into_diagnostic()?
    {
        artifect::format_lir_functions(
            formatter,
            &lir_functions,
            &function_names,
            &str_list,
            config.term_width,
        )
        .into_diagnostic()?;
    }

    // Optimization passes
    let mut lir_optimized_functions = lir::FunctionList::new();
    for (fid, f) in lir_functions.into_iter_id() {
        // To SSA
        let (f, dom_tree) = artifect::compile_ssa(f);

        if config
            .print(formatter, PrintFlag::FreshSsa)
            .into_diagnostic()?
        {
            artifect::format_ssa_blocks(
                formatter,
                fid,
                &f,
                &str_list,
                &function_names,
                config.term_width,
            )
            .into_diagnostic()?;
        }

        // Constant progpagation
        let f = artifect::constant_propagation(f);

        if config
            .print(formatter, PrintFlag::AfterConstantPropagation)
            .into_diagnostic()?
        {
            artifect::format_ssa_blocks(
                formatter,
                fid,
                &f,
                &str_list,
                &function_names,
                config.term_width,
            )
            .into_diagnostic()?;
        }

        // Dead Code Elimination
        let f = artifect::dead_code_elimination(f);

        if config
            .print(formatter, PrintFlag::AfterDeadCodeElimination)
            .into_diagnostic()?
        {
            artifect::format_ssa_blocks(
                formatter,
                fid,
                &f,
                &str_list,
                &function_names,
                config.term_width,
            )
            .into_diagnostic()?;
        }

        // Common Expression Elimination
        let f = artifect::common_expression_elimination(f, &dom_tree);

        if config
            .print(formatter, PrintFlag::AfterCommonExpressionElimination)
            .into_diagnostic()?
        {
            artifect::format_ssa_blocks(
                formatter,
                fid,
                &f,
                &str_list,
                &function_names,
                config.term_width,
            )
            .into_diagnostic()?;
        }

        // Back to LIR
        let f = artifect::deconstruct_ssa(f);

        if config
            .print(formatter, PrintFlag::BackToLir)
            .into_diagnostic()?
        {
            artifect::format_lir_function_optimized(
                formatter,
                &f,
                fid,
                &function_names,
                &str_list,
                config.term_width,
            )
            .into_diagnostic()?;
        }

        // Control Flow Simplification
        let f = artifect::control_flow_simplification(f);

        if config
            .print(formatter, PrintFlag::AfterControlFlowOptimization)
            .into_diagnostic()?
        {
            artifect::format_lir_function_optimized(
                formatter,
                &f,
                fid,
                &function_names,
                &str_list,
                config.term_width,
            )
            .into_diagnostic()?;
        }

        lir_optimized_functions.push(f);
    }

    if config
        .print(formatter, PrintFlag::AfterControlFlowOptimization)
        .into_diagnostic()?
    {
        artifect::format_lir_functions_optimized(
            formatter,
            &lir_optimized_functions,
            &function_names,
            &str_list,
            config.term_width,
        )
        .into_diagnostic()?;
    }

    // Compile byte-code
    let executable = artifect::compile_byte_code(lir_optimized_functions, str_list, main_fid)?;

    if config
        .print(formatter, PrintFlag::ByteCode)
        .into_diagnostic()?
    {
        let pretty_table = disassembler::disassemble(&executable);
        write!(formatter, "{pretty_table}").into_diagnostic()?;
    }

    Ok(executable)
}
