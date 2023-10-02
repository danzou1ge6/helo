use std::collections::HashMap;

use crate::{ir, lir};

pub struct Compiler {
    local_cnt: usize,
    temp_cnt: usize,
    arity: usize,
    fid: ir::FunctionId,
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
    fn new(local_cnt: usize, arity: usize, fid: ir::FunctionId) -> Self {
        Self {
            local_cnt,
            temp_cnt: local_cnt + 1,
            arity,
            fid,
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
    use ir::ExprNode::*;
    match ir_nodes[id].node() {
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
        ),
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
        ),
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
        ),
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
        ),
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
        ),
        Call { callee, args } => lower_call(
            to,
            *callee,
            args,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        ),
        Immediate(im) => lower_immediate(to, im.clone(), block),
        MakeClosure(fid, captures) => lower_make_closure(
            to,
            fid,
            captures,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            functions,
            compiler,
        ),
        Local(local) => lower_local(to, *local, block, compiler),
        ThisClosure(local) => lower_local(to, *local, block, compiler),
        UserFunction(fid) => {
            lower_user_function(to, fid, ir_nodes, ir_functions, builtins, block, functions)
        }
        Builtin(fid) => lower_builtin(to, fid, builtins, block),
        VariantField(local, pos) | TupleField(local, pos) => {
            lower_field(to, *local, *pos, block, compiler)
        }
        MakeTagged(tag, v) => lower_make_tagged(
            to,
            tag.code(),
            v,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        ),
        MakeTuple(v) => lower_make_tagged(
            to,
            0,
            v,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        ),
        Panic(s) => lower_panic(*s, block),
    }
}

pub fn lower_function<'s>(
    fid: &ir::FunctionId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    functions: &mut lir::FunctionTable,
) -> lir::FunctionId {
    if let Some(id) = functions.get(fid) {
        return id;
    }

    let f = ir_functions.get(fid).unwrap();
    let mut compiler = Compiler::new(f.local_cnt, f.arity, fid.clone());
    let mut blocks = lir::BlockHeap::new();
    let body = lower_expr_new_block(
        compiler.ret_temp(),
        f.body,
        ir_nodes,
        ir_functions,
        builtins,
        &mut blocks,
        functions,
        &mut compiler,
    );
    functions.insert(
        fid.clone(),
        lir::Function {
            body,
            blocks,
            arity: f.arity,
            meta: f.meta.clone(),
        },
    )
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
    operand: ir::ExprId,
    arms: &Vec<(ir::Tag<'s>, ir::ExprId)>,
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
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
        cases[tag.code() as usize] = *e;
    });

    let operand_temp = compiler.new_temp();
    let mut block = lower_expr(
        to,
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
    let cases = cases
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
        .collect();

    use lir::Instruction::*;
    block.push(JumpTable(operand_temp, cases));

    block
}

fn lower_switch<'s>(
    to: lir::TempId,
    operand: ir::ExprId,
    arms: &Vec<(ir::Immediate, ir::ExprId)>,
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::Block,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    use ir::Immediate;
    use lir::Instruction::*;

    let operand_temp = compiler.new_temp();
    let mut block = lower_expr(
        to,
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
            Immediate::Bool(b) => JumpIfEqBool(operand_temp, *b, e_block),
            Immediate::Int(i) => JumpIfEqInt(operand_temp, *i, e_block),
            Immediate::Str(s) => JumpIfEqStr(operand_temp, *s, e_block),
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
    block.push(JumpIfElse(test_temp, then_block, else_block));

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

    let args_temp: Vec<_> = args.iter().map(|_| compiler.new_temp()).collect();

    macro_rules! emit_arguments {
        ($block:ident) => {
            args.iter()
                .zip(args_temp.iter())
                .fold($block, |block, (arg, temp)| {
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
                })
        };
    }

    match &ir_nodes[callee].node() {
        // Optimization: we don't load a builtin and then apply arguments to it when we have enough arguments
        // Instead, we call the builtin directly.
        ir::ExprNode::Builtin(name) if args.len() == builtins.arity_by_name(name) => {
            let mut block = emit_arguments!(block);
            block.push(CallBuiltin(to, builtins.id_by_name(name), args_temp));
            block
        }
        // Optimization: we don't load a user function and then apply arguments to it if we have enough arguments.
        // Instead, we call this user function directly
        ir::ExprNode::UserFunction(name) if args.len() == ir_functions.get(name).unwrap().arity => {
            let mut block = emit_arguments!(block);
            let lir_fid = lower_function(name, ir_nodes, ir_functions, builtins, functions);
            block.push(Call(to, lir_fid, args_temp));
            block
        }
        // Optimization: Tall call of closure
        ir::ExprNode::ThisClosure(local) if args.len() == compiler.arity => {
            let callee_temp = compiler.local_id_to_temp(*local);
            let mut block = emit_arguments!(block);
            block.push(TailCall(to, callee_temp, args_temp));
            block
        }
        // Optimization: Tail call of non-closure function
        ir::ExprNode::UserFunction(fid) if *fid == compiler.fid && args.len() == compiler.arity => {
            let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, functions);
            let mut block = emit_arguments!(block);
            block.push(TailCallU(to, lir_fid, args_temp));
            block
        }
        _ => {
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
            let mut block = emit_arguments!(block);
            block.push(Apply(to, callee_temp, args_temp));
            block
        }
    }
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
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::Block {
    let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, functions);
    let temps: Vec<_> = captures
        .iter()
        .map(|local| compiler.local_id_to_temp(*local))
        .collect();

    use lir::Instruction::*;
    let f_temp = compiler.new_temp();
    block.push(Function(f_temp, lir_fid));
    block.push(Push(to, f_temp, temps));

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
    functions: &mut lir::FunctionTable,
) -> lir::Block {
    let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, functions);
    block.push(lir::Instruction::Function(to, lir_fid));
    block
}

fn lower_builtin<'s>(
    to: lir::TempId,
    fid: &ir::FunctionId,
    builtins: &lir::Builtins,
    mut block: lir::Block,
) -> lir::Block {
    let builtin_id = builtins.id_by_name(fid);
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

fn lower_make_tagged<'s>(
    to: lir::TempId,
    tag_code: u8,
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
