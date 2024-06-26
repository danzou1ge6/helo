use std::collections::HashMap;

use helo_parse::ast::BuiltinFunctionName;

use crate::{ir, lir};

pub struct Compiler {
    local_cnt: usize,
    temp_cnt: usize,
    non_capture_arity: usize,
}

impl Compiler {
    fn local_id_to_temp(&mut self, id: ir::LocalId) -> lir::TempId {
        lir::TempId(id.0)
    }
    fn new_temp(&mut self) -> lir::TempId {
        let r = lir::TempId(self.temp_cnt);
        self.temp_cnt += 1;
        r
    }
    fn new(local_cnt: usize, non_capture_arity: usize) -> Self {
        Self {
            local_cnt,
            temp_cnt: local_cnt + 1,
            non_capture_arity,
        }
    }
    fn ret_temp(&self) -> lir::TempId {
        lir::TempId(self.local_cnt)
    }
    fn temp_cnt(&self) -> usize {
        self.temp_cnt
    }
}

enum BlockEnd {
    Many(Vec<lir::BlockId>),
    Single(lir::BlockId),
}

impl From<lir::BlockId> for BlockEnd {
    fn from(value: lir::BlockId) -> Self {
        Self::Single(value)
    }
}

impl From<Vec<lir::BlockId>> for BlockEnd {
    fn from(value: Vec<lir::BlockId>) -> Self {
        Self::Many(value)
    }
}

fn lower_expr_untied<'s>(
    to: Option<lir::TempId>,
    id: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> BlockEnd {
    use ir::ExprNode::*;
    match ir_nodes[id].node() {
        Never => panic!("IR with Never nodes can never be lowered to LIR"),
        LetBind { local, value, in_ } => lower_let_bind(
            to,
            *local,
            *value,
            *in_,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        SwitchTag(operand, arms, default) => lower_switch_tag(
            to,
            *operand,
            arms,
            *default,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        Switch(operand, arms, default) => lower_switch(
            to,
            *operand,
            arms,
            *default,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        Cond(arms, default) => lower_cond(
            to,
            arms,
            *default,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        IfElse { test, then, else_ } => lower_if_else(
            to,
            *test,
            *then,
            *else_,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        Apply {
            callee,
            args,
            callee_impure,
        } => lower_apply(
            to.unwrap_or_else(|| compiler.new_temp()),
            *callee,
            *callee_impure,
            args,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        Immediate(im) => lower_immediate(
            to.unwrap_or_else(|| compiler.new_temp()),
            im.clone(),
            block,
            blocks,
        )
        .into(),
        MakeClosure(fid, captures) => lower_make_closure(
            to.unwrap_or_else(|| compiler.new_temp()),
            fid,
            captures,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        Local(local) => lower_local(
            to.unwrap_or_else(|| compiler.new_temp()),
            *local,
            block,
            blocks,
            compiler,
        )
        .into(),
        ThisClosure(local) => lower_local(
            to.unwrap_or_else(|| compiler.new_temp()),
            *local,
            block,
            blocks,
            compiler,
        )
        .into(),
        UserFunction(fid) => lower_user_function(
            to.unwrap_or_else(|| compiler.new_temp()),
            fid,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
        )
        .into(),
        Builtin(fid) => lower_builtin(
            to.unwrap_or_else(|| compiler.new_temp()),
            fid,
            builtins,
            block,
            blocks,
        )
        .into(),
        VariantField(local, pos) | TupleField(local, pos) => lower_field(
            to.unwrap_or_else(|| compiler.new_temp()),
            *local,
            *pos,
            block,
            blocks,
            compiler,
        )
        .into(),
        MakeTagged(tag, v) => lower_make_tagged(
            to.unwrap_or_else(|| compiler.new_temp()),
            tag.code(),
            v,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        MakeTuple(v) => lower_make_tagged(
            to.unwrap_or_else(|| compiler.new_temp()),
            0,
            v,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        Panic { file, span, msg } => lower_panic(*msg, *file, *span, block, blocks).into(),
        Assign(target, value) => lower_assign(
            *target,
            *value,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        Seq(exprs, result) => lower_seq(
            to,
            exprs,
            *result,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        If { test, then } => lower_if(
            *test,
            *then,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
        While { test, then } => lower_while(
            *test,
            *then,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        )
        .into(),
    }
}

fn lower_expr<'s>(
    to: Option<lir::TempId>,
    id: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let end = lower_expr_untied(
        to,
        id,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );
    match end {
        BlockEnd::Many(ends) => {
            let next_block = blocks.new_block();
            ends.into_iter().for_each(|b| {
                if !blocks[b].sealed() {
                    blocks.seal(b, lir::Jump::Jump(next_block))
                }
            });
            next_block
        }
        BlockEnd::Single(end) => end,
    }
}

fn lower_function_body<'s>(
    to: Option<lir::TempId>,
    id: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) {
    let end = lower_expr_untied(
        to,
        id,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );
    match end {
        BlockEnd::Many(ends) => {
            ends.into_iter().for_each(|b| {
                if !blocks[b].sealed() {
                    blocks.seal(b, lir::Jump::Ret(to))
                }
            });
        }
        BlockEnd::Single(end) => {
            if !blocks[end].sealed() {
                blocks.seal(end, lir::Jump::Ret(to))
            }
        }
    }
}

pub fn lower_function<'s>(
    fid: &ir::FunctionId<'s>,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
) -> lir::FunctionId {
    if let Some(id) = functions.get(fid) {
        return id;
    }

    functions.insert(fid.clone(), move |_, functions| {
        let f = ir_functions.get(&fid).unwrap();
        let mut compiler = Compiler::new(f.local_cnt, f.arity - f.capture_cnt);
        let mut blocks = lir::BlockHeap::new();

        let body = blocks.new_block();

        lower_function_body(
            if f.has_return {
                Some(compiler.ret_temp())
            } else {
                None
            },
            f.body,
            ir_nodes,
            ir_functions,
            builtins,
            body,
            &mut blocks,
            functions,
            &mut compiler,
        );

        lir::Function {
            body,
            blocks,
            arity: f.arity,
            meta: f.meta.clone(),
            name: f.name,
            temp_cnt: compiler.temp_cnt(),
        }
    })
}

fn lower_expr_new_block<'s>(
    to: Option<lir::TempId>,
    id: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> (lir::BlockId, lir::BlockId) {
    let begin = blocks.new_block();
    let end = lower_expr(
        to,
        id,
        ir_nodes,
        ir_functions,
        builtins,
        begin,
        blocks,
        functions,
        compiler,
    );
    (begin, end)
}

fn lower_assign<'s>(
    to: ir::LocalId,
    value: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let value_temp = compiler.new_temp();
    let block = lower_expr(
        Some(value_temp),
        value,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );
    blocks[block].push(lir::Instruction::Mov(
        compiler.local_id_to_temp(to),
        value_temp,
    ));
    block
}

fn lower_seq<'s>(
    to: Option<lir::TempId>,
    exprs: &[ir::ExprId],
    result: Option<ir::ExprId>,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let mut block = exprs.iter().copied().fold(block, |block, expr| {
        let block = lower_expr(
            None,
            expr,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        );
        block
    });

    if let Some(result) = result {
        block = lower_expr(
            to,
            result,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        );
    }

    block
}

fn lower_if<'s>(
    test: ir::ExprId,
    then: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> Vec<lir::BlockId> {
    let test_temp = compiler.new_temp();
    let block = lower_expr(
        Some(test_temp),
        test,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );

    let else_branch = blocks.new_block();
    let (then_branch, then_branch_end) = lower_expr_new_block(
        None,
        then,
        ir_nodes,
        ir_functions,
        builtins,
        blocks,
        functions,
        compiler,
    );

    blocks.seal(
        block,
        lir::Jump::JumpIfElse(test_temp, then_branch, else_branch),
    );
    vec![then_branch_end, else_branch]
}

fn lower_while<'s>(
    test: ir::ExprId,
    then: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let test_temp = compiler.new_temp();
    let test_block = blocks.new_block();

    blocks.seal(block, lir::Jump::Jump(test_block));

    let test_block = lower_expr(
        Some(test_temp),
        test,
        ir_nodes,
        ir_functions,
        builtins,
        test_block,
        blocks,
        functions,
        compiler,
    );

    let else_branch = blocks.new_block();
    let (then_branch, then_branch_end) = lower_expr_new_block(
        None,
        then,
        ir_nodes,
        ir_functions,
        builtins,
        blocks,
        functions,
        compiler,
    );

    blocks.seal(
        test_block,
        lir::Jump::JumpIfElse(test_temp, then_branch, else_branch),
    );

    blocks.seal(then_branch_end, lir::Jump::Jump(test_block));

    else_branch
}
fn lower_let_bind<'s>(
    to: Option<lir::TempId>,
    local: ir::LocalId,
    value: ir::ExprId,
    in_: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let temp_id = compiler.local_id_to_temp(local);
    let block = lower_expr(
        Some(temp_id),
        value,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );
    let block = lower_expr(
        to,
        in_,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );
    block
}

fn lower_switch_tag<'s>(
    to: Option<lir::TempId>,
    operand: ir::ExprId,
    arms: &Vec<(ir::Tag<'s>, ir::ExprId)>,
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> Vec<lir::BlockId> {
    if arms.len() == 0 {
        let block = lower_expr(
            to,
            default,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        );
        return vec![block];
    }

    let tag_cnt = arms.first().unwrap().0.possibilities();
    let mut cases: Vec<_> = (0..tag_cnt).map(|_| default).collect();
    arms.iter().for_each(|(tag, e)| {
        cases[tag.code() as usize] = *e;
    });

    let operand_temp = compiler.new_temp();
    let block = lower_expr(
        Some(operand_temp),
        operand,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );

    let mut expr_map = HashMap::new();
    let (branches, branches_end) = cases
        .into_iter()
        .map(|e| {
            *expr_map.entry(e).or_insert_with(|| {
                lower_expr_new_block(
                    to,
                    e,
                    ir_nodes,
                    ir_functions,
                    builtins,
                    blocks,
                    functions,
                    compiler,
                )
            })
        })
        .unzip();

    blocks.seal(block, lir::Jump::JumpTable(operand_temp, branches));
    branches_end
}

fn lower_switch<'s>(
    to: Option<lir::TempId>,
    operand: ir::ExprId,
    arms: &[(ir::Immediate, ir::ExprId)],
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> Vec<lir::BlockId> {
    let operand_temp = compiler.new_temp();
    let block = lower_expr(
        Some(operand_temp),
        operand,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );

    let mut expr_map = HashMap::new();
    let (branches, mut branches_end) = arms
        .iter()
        .map(|(_, e)| {
            *expr_map.entry(e).or_insert_with(|| {
                lower_expr_new_block(
                    to,
                    *e,
                    ir_nodes,
                    ir_functions,
                    builtins,
                    blocks,
                    functions,
                    compiler,
                )
            })
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    let (default_branch, default_branch_end) = *expr_map.entry(&default).or_insert_with(|| {
        lower_expr_new_block(
            to,
            default,
            ir_nodes,
            ir_functions,
            builtins,
            blocks,
            functions,
            compiler,
        )
    });

    match arms.first().unwrap().0 {
        ir::Immediate::Int(_) => {
            let cases = arms
                .iter()
                .map(|(value, _)| value.clone().unwrap_int())
                .zip(branches.into_iter())
                .collect();
            blocks.seal(
                block,
                lir::Jump::JumpSwitchInt(operand_temp, cases, default_branch),
            );
        }
        ir::Immediate::Bool(b) => {
            let (true_branch, false_branch) = if b {
                (branches[0], branches[1])
            } else {
                (branches[1], branches[0])
            };
            blocks.seal(
                block,
                lir::Jump::JumpIfElse(operand_temp, true_branch, false_branch),
            );
        }
        ir::Immediate::Str(_) => {
            let cases = arms
                .iter()
                .map(|(value, _)| value.clone().unwrap_str())
                .zip(branches.into_iter())
                .collect();
            blocks.seal(
                block,
                lir::Jump::JumpSwitchStr(operand_temp, cases, default_branch),
            );
        }
        ir::Immediate::Char(_) => {
            let cases = arms
                .iter()
                .map(|(value, _)| value.clone().unwrap_char())
                .zip(branches.into_iter())
                .collect();
            blocks.seal(
                block,
                lir::Jump::JumpSwitchChar(operand_temp, cases, default_branch),
            );
        }
        ir::Immediate::Float(_) => unreachable!(),
    }

    branches_end.push(default_branch_end);
    branches_end
}

fn lower_cond<'s>(
    to: Option<lir::TempId>,
    arms: &[(ir::ExprId, ir::ExprId)],
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> Vec<lir::BlockId> {
    let mut expr_map = HashMap::new();

    let mut branches_end = Vec::new();
    let block = arms.iter().fold(block, |block, (cond, e)| {
        let test = compiler.new_temp();
        let block = lower_expr(
            Some(test),
            *cond,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        );

        let (branch, branch_end) = *expr_map.entry(e).or_insert_with(|| {
            lower_expr_new_block(
                to,
                *e,
                ir_nodes,
                ir_functions,
                builtins,
                blocks,
                functions,
                compiler,
            )
        });

        let next_block = blocks.new_block();

        blocks.seal(block, lir::Jump::JumpIfElse(test, branch, next_block));
        branches_end.push(branch_end);
        next_block
    });

    let default_branch_end = lower_expr(
        to,
        default,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );

    branches_end.push(default_branch_end);
    branches_end
}

fn lower_if_else<'s>(
    to: Option<lir::TempId>,
    test: ir::ExprId,
    then: ir::ExprId,
    else_: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> Vec<lir::BlockId> {
    let test_temp = compiler.new_temp();
    let block = lower_expr(
        Some(test_temp),
        test,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );

    let (then_branch, then_branch_end) = lower_expr_new_block(
        to,
        then,
        ir_nodes,
        ir_functions,
        builtins,
        blocks,
        functions,
        compiler,
    );
    let (else_branch, else_branch_end) = lower_expr_new_block(
        to,
        else_,
        ir_nodes,
        ir_functions,
        builtins,
        blocks,
        functions,
        compiler,
    );

    blocks.seal(
        block,
        lir::Jump::JumpIfElse(test_temp, then_branch, else_branch),
    );

    vec![then_branch_end, else_branch_end]
}

fn lower_apply<'s>(
    to: lir::TempId,
    callee: ir::ExprId,
    callee_impure: bool,
    args: &[ir::ExprId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> lir::BlockId {
    use lir::Instruction::*;

    let args_temp: Vec<_> = args.iter().map(|_| compiler.new_temp()).collect();

    macro_rules! emit_arguments {
        ($block:ident) => {
            args.iter()
                .zip(args_temp.iter())
                .fold($block, |block, (arg, temp)| {
                    lower_expr(
                        Some(*temp),
                        *arg,
                        ir_nodes,
                        ir_functions,
                        builtins,
                        block,
                        blocks,
                        functions,
                        compiler,
                    )
                })
        };
    }

    match &ir_nodes[callee].node() {
        // Optimization: we don't load a builtin and then apply arguments to it when we have enough arguments
        // Instead, we call the builtin directly.
        ir::ExprNode::Builtin(name) if args.len() == builtins.arity_by_name(name) => {
            let block = emit_arguments!(block);

            let inst = if callee_impure {
                CallBuiltinImpure
            } else {
                CallBuiltin
            };

            blocks[block].push(inst(to, builtins.id_by_name(name), args_temp));
            block
        }
        // Optimization: we don't load a user function and then apply arguments to it if we have enough arguments.
        // Instead, we call this user function directly
        ir::ExprNode::UserFunction(f_id) if args.len() == ir_functions.get(f_id).unwrap().arity => {
            let block = emit_arguments!(block);
            let lir_fid = lower_function(f_id, ir_nodes, ir_functions, builtins, functions);

            let inst = if callee_impure { CallImpure } else { Call };

            blocks[block].push(inst(to, lir_fid, args_temp));
            block
        }
        // Optimization: Call of the executing closure
        ir::ExprNode::ThisClosure(local) if args.len() == compiler.non_capture_arity => {
            let callee_temp = compiler.local_id_to_temp(*local);
            let block = emit_arguments!(block);

            let inst = if callee_impure {
                CallThisClosureImpure
            } else {
                CallThisClosure
            };

            blocks[block].push(inst(to, callee_temp, args_temp));
            block
        }
        _ => {
            let callee_temp = compiler.new_temp();
            let block = lower_expr(
                Some(callee_temp),
                callee,
                ir_nodes,
                ir_functions,
                builtins,
                block,
                blocks,
                functions,
                compiler,
            );
            let block = emit_arguments!(block);

            let inst = if callee_impure { ApplyImpure } else { Apply };

            blocks[block].push(inst(to, callee_temp, args_temp));
            block
        }
    }
}

fn lower_immediate<'s>(
    to: lir::TempId,
    imme: ir::Immediate,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
) -> lir::BlockId {
    use ir::Immediate;
    use lir::Instruction::*;

    let inst = match imme {
        Immediate::Int(i) => Int(to, i),
        Immediate::Float(f) => Float(to, f),
        Immediate::Str(s) => Str(to, s),
        Immediate::Bool(b) => Bool(to, b),
        Immediate::Char(c) => Char(to, c),
    };

    blocks[block].push(inst);
    block
}

fn lower_make_closure<'s>(
    to: lir::TempId,
    fid: &ir::FunctionId<'s>,
    captures: &[ir::LocalId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, functions);
    let temps: Vec<_> = captures
        .iter()
        .map(|local| compiler.local_id_to_temp(*local))
        .collect();

    use lir::Instruction::*;
    blocks[block].push(Function(to, lir_fid));
    blocks[block].push(AddToEnv(to, to, temps));
    block
}

fn lower_local<'s>(
    to: lir::TempId,
    local: ir::LocalId,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    compiler: &mut Compiler,
) -> lir::BlockId {
    blocks[block].push(lir::Instruction::Mov(to, compiler.local_id_to_temp(local)));
    block
}

fn lower_user_function<'s>(
    to: lir::TempId,
    fid: &ir::FunctionId<'s>,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
) -> lir::BlockId {
    let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, functions);
    blocks[block].push(lir::Instruction::Function(to, lir_fid));
    block
}

fn lower_builtin<'s>(
    to: lir::TempId,
    name: &BuiltinFunctionName,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
) -> lir::BlockId {
    let builtin_id = builtins.id_by_name(name);
    blocks[block].push(lir::Instruction::Buitltin(to, builtin_id));
    block
}

fn lower_field<'s>(
    to: lir::TempId,
    local: ir::LocalId,
    field: usize,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    compiler: &mut Compiler,
) -> lir::BlockId {
    blocks[block].push(lir::Instruction::Field(
        to,
        compiler.local_id_to_temp(local),
        field,
    ));
    block
}

fn lower_make_tagged<'s>(
    to: lir::TempId,
    tag_code: u8,
    fields: &[ir::ExprId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &ir::BuiltinTable<'s>,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable<'s, lir::Function>,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let fields_temp: Vec<_> = fields.iter().map(|_| compiler.new_temp()).collect();
    let block = fields
        .iter()
        .copied()
        .zip(fields_temp.iter().copied())
        .fold(block, |b, (f, t)| {
            lower_expr(
                Some(t),
                f,
                ir_nodes,
                ir_functions,
                builtins,
                b,
                blocks,
                functions,
                compiler,
            )
        });

    blocks[block].push(lir::Instruction::Tagged(to, tag_code, fields_temp));
    block
}

fn lower_panic<'s>(
    msg: ir::StrId,
    file: ir::StrId,
    span: (usize, usize),
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
) -> lir::BlockId {
    blocks.seal(block, lir::Jump::Panic { file, msg, span });
    block
}
