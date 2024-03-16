use helo_ir::{artifect, ir, lir};
use helo_parse::{ast, errors, source_tree};
use helo_runtime::{disassembler, executable};

use miette::IntoDiagnostic;

#[cfg(feature = "stage-value-enum")]
use clap::ValueEnum;

#[cfg(feature = "stage-wasm-bindgen")]
use wasm_bindgen::prelude::wasm_bindgen;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "stage-value-enum", derive(ValueEnum), value(rename_all = "kebab-case"))]
#[cfg_attr(feature = "stage-wasm-bindgen", wasm_bindgen)]
pub enum Stage {
    InferedType,
    Ir,
    AfterInline,
    FreshLir,
    FreshSsa,
    AfterConstantPropagation,
    AfterDeadCodeElimination,
    AfterCommonExpressionElimination,
    BackToLir,
    AfterControlFlowOptimization,
    ByteCode,
}

impl Stage {
    pub fn name(&self) -> &'static str {
        use Stage::*;
        match self {
            InferedType => "---- Infered Type ----\n",
            Ir => "---- IR ----\n",
            AfterInline => "---- After Inline Optimization ----\n",
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

pub struct SingleOutController<O>
where
    O: std::io::Write,
{
    pub produce_exe: bool,
    pub print_stages: Vec<Stage>,
    pub print_all: bool,
    pub term_width: usize,
    pub out: O,
}

impl<O> SingleOutController<O>
where
    O: std::io::Write,
{
    fn print_header(&self) -> bool {
        self.print_stages.len() > 1
    }
    fn abort(&self, stage: Stage) -> bool {
        if self.produce_exe || self.print_all {
            return false;
        }
        if let Some(last_stage) = self.print_stages.iter().max() {
            *last_stage <= stage
        } else {
            true
        }
    }
}

impl<O> CompileController for SingleOutController<O>
where
    O: std::io::Write,
{
    fn infered_type(
        &mut self,
        typed_symbols: &helo_parse::typed::Symbols<'_>,
    ) -> miette::Result<bool> {
        if self.print_header() {
            write!(&mut self.out, "{}", Stage::InferedType.name()).into_diagnostic()?;
        }
        for (name, f) in typed_symbols.functions.iter() {
            write!(
                &mut self.out,
                "{}: {}\n",
                name.to_string(&typed_symbols.instances),
                f.type_
            )
            .into_diagnostic()?;
        }
        Ok(self.abort(Stage::InferedType))
    }

    fn ir<'s>(
        &mut self,
        ir_functions: &helo_ir::ir::FunctionTable<'s>,
        ir_nodes: &helo_ir::ir::ExprHeap<'s>,
        str_list: &ir::StrList,
        typed_symbols: &helo_parse::typed::Symbols<'_>,
    ) -> miette::Result<bool> {
        if self.print_header() {
            write!(&mut self.out, "{}", Stage::Ir.name()).into_diagnostic()?;
        }
        artifect::format_ir_functions(
            &mut self.out,
            ir_functions,
            ir_nodes,
            str_list,
            &typed_symbols.instances,
            self.term_width,
        )
        .into_diagnostic()?;
        Ok(self.abort(Stage::Ir))
    }

    fn after_inline<'s>(
        &mut self,
        ir_functions: &helo_ir::ir::FunctionTable<'s>,
        ir_nodes: &helo_ir::ir::ExprHeap<'s>,
        str_list: &ir::StrList,
        typed_symbols: &helo_parse::typed::Symbols<'_>,
    ) -> miette::Result<bool> {
        if self.print_header() {
            write!(&mut self.out, "{}", Stage::AfterInline.name()).into_diagnostic()?;
        }
        artifect::format_ir_functions(
            &mut self.out,
            ir_functions,
            ir_nodes,
            str_list,
            &typed_symbols.instances,
            self.term_width,
        )
        .into_diagnostic()?;

        Ok(self.abort(Stage::AfterInline))
    }

    fn fresh_lir<'s>(
        &mut self,
        lir_functions: &lir::FunctionList<lir::Function>,
        function_names: &lir::FunctionNameList,
        str_list: &ir::StrList,
    ) -> miette::Result<bool> {
        if self.print_header() {
            write!(&mut self.out, "{}", Stage::FreshLir.name()).into_diagnostic()?;
        }
        artifect::format_lir_functions(
            &mut self.out,
            lir_functions,
            function_names,
            str_list,
            self.term_width,
        )
        .into_diagnostic()?;
        Ok(self.abort(Stage::FreshLir))
    }

    fn fresh_ssa<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::ssa::Function,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()> {
        if self.print_header() {
            write!(&mut self.out, "{}", Stage::FreshSsa.name()).into_diagnostic()?;
        }
        artifect::format_ssa_blocks(
            &mut self.out,
            *fid,
            f,
            str_list,
            function_names,
            self.term_width,
        )
        .into_diagnostic()?;
        Ok(())
    }

    fn after_constant_propagation<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::ssa::Function,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()> {
        if self.print_header() {
            write!(
                &mut self.out,
                "{}",
                Stage::AfterConstantPropagation.name()
            )
            .into_diagnostic()?;
        }
        artifect::format_ssa_blocks(
            &mut self.out,
            *fid,
            f,
            str_list,
            function_names,
            self.term_width,
        )
        .into_diagnostic()?;
        Ok(())
    }

    fn after_dead_code_elimination<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::ssa::Function,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()> {
        if self.print_header() {
            write!(
                &mut self.out,
                "{}",
                Stage::AfterDeadCodeElimination.name()
            )
            .into_diagnostic()?;
        }
        artifect::format_ssa_blocks(
            &mut self.out,
            *fid,
            f,
            str_list,
            function_names,
            self.term_width,
        )
        .into_diagnostic()?;
        Ok(())
    }

    fn after_common_expression_elimination<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::ssa::Function,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()> {
        if self.print_header() {
            write!(
                &mut self.out,
                "{}",
                Stage::AfterCommonExpressionElimination.name()
            )
            .into_diagnostic()?;
        }
        artifect::format_ssa_blocks(
            &mut self.out,
            *fid,
            f,
            str_list,
            function_names,
            self.term_width,
        )
        .into_diagnostic()?;
        Ok(())
    }

    fn back_to_lir<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::FunctionOptimized,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()> {
        if self.print_header() {
            write!(&mut self.out, "{}", Stage::BackToLir.name()).into_diagnostic()?;
        }
        artifect::format_lir_function_optimized(
            &mut self.out,
            f,
            *fid,
            function_names,
            str_list,
            self.term_width,
        )
        .into_diagnostic()?;

        Ok(())
    }

    fn after_control_flow_simplification<'s>(
        &mut self,
        lir_functions: &lir::FunctionList<lir::FunctionOptimized>,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<bool> {
        if self.print_header() {
            write!(
                &mut self.out,
                "{}",
                Stage::AfterControlFlowOptimization.name()
            )
            .into_diagnostic()?;
        }
        artifect::format_lir_functions_optimized(
            &mut self.out,
            lir_functions,
            function_names,
            str_list,
            self.term_width,
        )
        .into_diagnostic()?;
        Ok(self.abort(Stage::AfterControlFlowOptimization))
    }

    fn byte_code<'s>(&mut self, exe: &executable::Executable) -> miette::Result<()> {
        if self.print_header() {
            write!(&mut self.out, "{}", Stage::ByteCode.name()).into_diagnostic()?;
        }
        let pretty_table = disassembler::disassemble(exe);
        write!(&mut self.out, "{pretty_table}").into_diagnostic()?;
        Ok(())
    }
}

pub trait CompileController {
    fn infered_type(
        &mut self,
        typed_symbols: &helo_parse::typed::Symbols<'_>,
    ) -> miette::Result<bool>;
    fn ir<'s>(
        &mut self,
        ir_functions: &helo_ir::ir::FunctionTable<'s>,
        ir_nodes: &helo_ir::ir::ExprHeap<'s>,
        str_list: &ir::StrList,
        typed_symbols: &helo_parse::typed::Symbols<'_>,
    ) -> miette::Result<bool>;
    fn after_inline<'s>(
        &mut self,
        ir_functions: &helo_ir::ir::FunctionTable<'s>,
        ir_nodes: &helo_ir::ir::ExprHeap<'s>,
        str_list: &ir::StrList,
        typed_symbols: &helo_parse::typed::Symbols<'_>,
    ) -> miette::Result<bool>;
    fn fresh_lir<'s>(
        &mut self,
        lir_functions: &lir::FunctionList<lir::Function>,
        function_names: &lir::FunctionNameList,
        str_list: &ir::StrList,
    ) -> miette::Result<bool>;
    fn fresh_ssa<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::ssa::Function,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()>;
    fn after_constant_propagation<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::ssa::Function,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()>;
    fn after_dead_code_elimination<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::ssa::Function,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()>;
    fn after_common_expression_elimination<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::ssa::Function,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()>;
    fn back_to_lir<'s>(
        &mut self,
        fid: &lir::FunctionId,
        f: &lir::FunctionOptimized,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<()>;
    fn after_control_flow_simplification<'s>(
        &mut self,
        lir_functions: &lir::FunctionList<lir::FunctionOptimized>,
        str_list: &ir::StrList,
        function_names: &lir::FunctionNameList,
    ) -> miette::Result<bool>;
    fn byte_code<'s>(&mut self, exe: &executable::Executable) -> miette::Result<()>;
}

pub fn compile(
    src_tree: &source_tree::SourceTree,
    mut controller: impl CompileController,
) -> miette::Result<Option<executable::Executable>> {
    // Parse
    let (ast_symbols, ast_nodes) = artifect::parse(src_tree)?;

    // Type inference and lower to IR
    let mut e = errors::ManyError::new();
    let (typed_symbols, typed_nodes) = artifect::infer_type(ast_symbols, ast_nodes, &mut e);

    if controller.infered_type(&typed_symbols)? {
        return Ok(None);
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
    let (ir_functions, mut ir_nodes, str_list, _) =
        artifect::compiler_ir([main.clone()].iter(), &typed_symbols, typed_nodes, &mut e);
    let _ = e.emit()?;

    if controller.ir(&ir_functions, &ir_nodes, &str_list, &typed_symbols)? {
        return Ok(None);
    }

    // Do inline optimization
    let ir_functions = artifect::inline_ir(ir_functions, &mut ir_nodes, 16, [main.clone()].iter());

    if controller.after_inline(&ir_functions, &ir_nodes, &str_list, &typed_symbols)? {
        return Ok(None);
    }

    // Lower to LIR
    let lir_functions = artifect::compile_lir(ir_functions, ir_nodes);
    let main_fid = lir_functions
        .get(&ir::FunctionId::of_non_generic(main))
        .unwrap();
    let lir_functions = lir_functions.to_list();
    let function_names = lir_functions.function_name_list();

    if controller.fresh_lir(&lir_functions, &function_names, &str_list)? {
        return Ok(None);
    }

    // Optimization passes
    let mut lir_optimized_functions = lir::FunctionList::new();
    for (fid, f) in lir_functions.into_iter_id() {
        // To SSA
        let (f, dom_tree) = artifect::compile_ssa(f);

        controller.fresh_ssa(&fid, &f, &str_list, &function_names)?;

        // Constant progpagation
        let f = artifect::constant_propagation(f);

        controller.after_constant_propagation(&fid, &f, &str_list, &function_names)?;

        // Dead Code Elimination
        let f = artifect::dead_code_elimination(f);

        controller.after_dead_code_elimination(&fid, &f, &str_list, &function_names)?;

        // Common Expression Elimination
        let f = artifect::common_expression_elimination(f, &dom_tree);

        controller.after_common_expression_elimination(&fid, &f, &str_list, &function_names)?;

        // Back to LIR
        let f = artifect::deconstruct_ssa(f);

        controller.back_to_lir(&fid, &f, &str_list, &function_names)?;

        lir_optimized_functions.push(f);
    }

    if controller.after_control_flow_simplification(
        &lir_optimized_functions,
        &str_list,
        &function_names,
    )? {
        return Ok(None);
    }

    // Compile byte-code
    let executable = artifect::compile_byte_code(lir_optimized_functions, str_list, main_fid)?;

    controller.byte_code(&executable)?;

    Ok(Some(executable))
}
