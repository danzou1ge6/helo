use std::collections::HashMap;

use crate::{ir, lir};

pub struct Compiler {
    local_cnt: usize,
    temp_cnt: usize,
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
    fn with_local_cnt(local_cnt: usize) -> Self {
        Self {
            local_cnt,
            temp_cnt: local_cnt + 1,
        }
    }
    fn ret_temp(&self) -> lir::TempId {
        lir::TempId(self.local_cnt)
    }
}

fn lower_expr<'s>(
    to: lir::TempId,
    id: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    unimplemented!()
}

fn lower_function<'s>(
    fid: &ir::FunctionId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
) -> lir::FunctionId {
    if let Some(id) = functions.get(fid) {
        return id;
    }

    let f = ir_functions.get(fid).unwrap();
    let mut compiler = Compiler::with_local_cnt(f.local_cnt);
    let body = lower_expr_new_block(
        compiler.ret_temp(),
        f.body,
        ir_nodes,
        ir_functions,
        builtins,
        blocks,
        functions,
        &mut compiler,
    );
    functions.insert(fid.clone(), lir::Function { body })
}

fn lower_expr_new_block<'s>(
    to: lir::TempId,
    id: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let block = lir::Block::new();
    let block = lower_expr(
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
    blocks.push(block)
}

fn lower_let_bind<'s>(
    to: lir::TempId,
    local: ir::LocalId,
    value: ir::ExprId,
    in_: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    let temp_id = compiler.local_id_to_temp(local);
    let block = lower_expr(
        temp_id,
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
    to: lir::TempId,
    operand: ir::LocalId,
    arms: &Vec<(ir::Tag<'s>, ir::ExprId)>,
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    mut block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
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
        return block;
    }

    let tag_cnt = arms.first().unwrap().0.possibilities();
    let mut cases: Vec<_> = (0..tag_cnt).map(|_| default).collect();
    arms.iter().for_each(|(tag, e)| {
        cases[tag.code()] = *e;
    });

    let operand = compiler.local_id_to_temp(operand);

    let mut expr_map = HashMap::new();
    let cases = cases.into_iter().map(|e| {
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
    });

    use lir::Instruction::*;
    block.push(JunpTable(operand));
    cases.for_each(|b| block.push(Jump(b)));

    block
}

fn lower_switch<'s>(
    to: lir::TempId,
    operand: ir::LocalId,
    arms: &Vec<(ir::Immediate, ir::ExprId)>,
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    mut block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    use ir::Immediate;
    use lir::Instruction::*;

    let operand = compiler.local_id_to_temp(operand);

    let mut expr_map = HashMap::new();

    arms.iter().for_each(|(value, e)| {
        let e_block = *expr_map.entry(e).or_insert_with(|| {
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

        block.push(match value {
            Immediate::Bool(b) => JumpIfEqBool(operand, *b, e_block),
            Immediate::Int(i) => JumpIfEqInt(operand, *i, e_block),
            Immediate::Str(s) => JumpIfEqStr(operand, *s, e_block),
            Immediate::Float(_) => unreachable!(),
        });
    });

    let default_block = *expr_map.entry(&default).or_insert_with(|| {
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

    block.push(Jump(default_block));

    block
}

fn lower_cond<'s>(
    to: lir::TempId,
    arms: &[(ir::ExprId, ir::ExprId)],
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    let mut expr_map = HashMap::new();

    use lir::Instruction::*;

    let mut block = arms.iter().fold(block, |mut block, (cond, e)| {
        let test = compiler.new_temp();
        block = lower_expr(
            test,
            *cond,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        );

        let e_block = *expr_map.entry(e).or_insert_with(|| {
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

        block.push(JumpIf(test, e_block));
        block
    });

    let default_block = *expr_map.entry(&default).or_insert_with(|| {
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
    block.push(Jump(default_block));

    block
}

fn lower_if_else<'s>(
    to: lir::TempId,
    test: ir::ExprId,
    then: ir::ExprId,
    else_: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    let test_temp = compiler.new_temp();
    let mut block = lower_expr(
        test_temp,
        test,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );

    let then_block = lower_expr_new_block(
        to,
        then,
        ir_nodes,
        ir_functions,
        builtins,
        blocks,
        functions,
        compiler,
    );
    let else_block = lower_expr_new_block(
        to,
        else_,
        ir_nodes,
        ir_functions,
        builtins,
        blocks,
        functions,
        compiler,
    );

    use lir::Instruction::*;
    block.push(JumpIf(test_temp, then_block));
    block.push(Jump(else_block));

    block
}

fn lower_call<'s>(
    to: lir::TempId,
    callee: ir::ExprId,
    args: &[ir::ExprId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    use lir::Instruction::*;

    let callee_temp = compiler.new_temp();
    let block = lower_expr(
        callee_temp,
        callee,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    );

    let args_temp: Vec<_> = args.iter().map(|_| compiler.new_temp()).collect();
    let mut block = args
        .iter()
        .zip(args_temp.iter())
        .fold(block, |block, (arg, temp)| {
            lower_expr(
                *temp,
                *arg,
                ir_nodes,
                ir_functions,
                builtins,
                block,
                blocks,
                functions,
                compiler,
            )
        });

    block.push(Apply {
        callee: callee_temp,
        ret: to,
        args: args_temp,
    });

    block
}

fn lower_immediate<'s>(to: lir::TempId, imme: ir::Immediate, mut block: lir::Block) -> lir::Block {
    use ir::Immediate;
    use lir::Instruction::*;

    let inst = match imme {
        Immediate::Int(i) => Int(to, i),
        Immediate::Float(f) => Float(to, f),
        Immediate::Str(s) => Str(to, s),
        Immediate::Bool(b) => Bool(to, b),
    };

    block.push(inst);
    block
}

fn lower_make_closure<'s>(
    to: lir::TempId,
    fid: &ir::FunctionId,
    captures: &[ir::LocalId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    mut block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, blocks, functions);
    let temps: Vec<_> = captures
        .iter()
        .map(|local| compiler.local_id_to_temp(*local))
        .collect();

    use lir::Instruction::*;
    block.push(Function(to, lir_fid));
    block.push(Push(to, temps));

    block
}

fn lower_local<'s>(
    to: lir::TempId,
    local: ir::LocalId,
    mut block: lir::Block,
    compiler: &mut Compiler,
) -> lir::Block {
    block.push(lir::Instruction::Mov(to, compiler.local_id_to_temp(local)));
    block
}

fn lower_user_function<'s>(
    to: lir::TempId,
    fid: &ir::FunctionId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    mut block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
) -> lir::Block {
    let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, blocks, functions);
    block.push(lir::Instruction::Function(to, lir_fid));
    block
}

fn lower_builtin<'s>(
    to: lir::TempId,
    fid: &ir::FunctionId,
    builtins: &lir::Builtins,
    mut block: lir::Block,
) -> lir::Block {
    let builtin_id = builtins.get_by_name(fid);
    block.push(lir::Instruction::Buitltin(to, builtin_id));
    block
}

fn lower_field<'s>(
    to: lir::TempId,
    local: ir::LocalId,
    field: usize,
    mut block: lir::Block,
    compiler: &mut Compiler,
) -> lir::Block {
    block.push(lir::Instruction::Field(
        to,
        compiler.local_id_to_temp(local),
        field,
    ));
    block
}

fn lower_make_variant<'s>(
    to: lir::TempId,
    tag: &ir::Tag<'s>,
    fields: &[ir::ExprId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    lower_make_tagged(
        to,
        tag.code(),
        fields,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    )
}
fn lower_make_tuple<'s>(
    to: lir::TempId,
    fields: &[ir::ExprId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    lower_make_tagged(
        to,
        0,
        fields,
        ir_nodes,
        ir_functions,
        builtins,
        block,
        blocks,
        functions,
        compiler,
    )
}

fn lower_make_tagged<'s>(
    to: lir::TempId,
    tag_code: usize,
    fields: &[ir::ExprId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    let fields_temp: Vec<_> = fields.iter().map(|_| compiler.new_temp()).collect();
    let mut block = fields
        .iter()
        .copied()
        .zip(fields_temp.iter().copied())
        .fold(block, |b, (f, t)| {
            lower_expr(
                t,
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

    block.push(lir::Instruction::Tagged(to, tag_code, fields_temp));
    block
}

fn lower_panic<'s>(sid: ir::StrId, mut block: lir::Block) -> lir::Block {
    block.push(lir::Instruction::Panic(sid));
    block
}
