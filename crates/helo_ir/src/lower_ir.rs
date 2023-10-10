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
    fn temp_cnt(&self) -> usize {
        self.temp_cnt
    }
}

fn tie_loose_ends(ends: Vec<lir::BlockId>, blocks: &mut lir::BlockHeap) -> lir::BlockId {
    let next_block = blocks.new_block();
    ends.into_iter()
        .for_each(|b| blocks.seal(b, lir::Jump::Jump(next_block)));
    next_block
}

fn lower_expr<'s>(
    to: lir::TempId,
    id: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::BlockId {
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
        SwitchTag(operand, arms, default) => {
            let loose_ends = lower_switch_tag(
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
            );
            tie_loose_ends(loose_ends, blocks)
        }
        Switch(operand, arms, default) => {
            let loose_ends = lower_switch(
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
            );
            tie_loose_ends(loose_ends, blocks)
        }
        Cond(arms, default) => {
            let loose_ends = lower_cond(
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
            );
            tie_loose_ends(loose_ends, blocks)
        }
        IfElse { test, then, else_ } => {
            let loose_ends = lower_if_else(
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
            );
            tie_loose_ends(loose_ends, blocks)
        }
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
        Immediate(im) => lower_immediate(to, im.clone(), block, blocks),
        MakeClosure(fid, captures) => lower_make_closure(
            to,
            fid,
            captures,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
            compiler,
        ),
        Local(local) => lower_local(to, *local, block, blocks, compiler),
        ThisClosure(local) => lower_local(to, *local, block, blocks, compiler),
        UserFunction(fid) => lower_user_function(
            to,
            fid,
            ir_nodes,
            ir_functions,
            builtins,
            block,
            blocks,
            functions,
        ),
        Builtin(fid) => lower_builtin(to, fid, builtins, block, blocks),
        VariantField(local, pos) | TupleField(local, pos) => {
            lower_field(to, *local, *pos, block, blocks, compiler)
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
        Panic(s) => lower_panic(*s, block, blocks),
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

    functions.insert(fid.clone(), move |_, functions| {
        let f = ir_functions.get(fid).unwrap();
        let mut compiler = Compiler::new(f.local_cnt, f.arity, fid.clone());
        let mut blocks = lir::BlockHeap::new();

        let (body, ret_block) = lower_expr_new_block(
            compiler.ret_temp(),
            f.body,
            ir_nodes,
            ir_functions,
            builtins,
            &mut blocks,
            functions,
            &mut compiler,
        );
        blocks.seal(ret_block, lir::Jump::Ret(compiler.ret_temp()));

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
    to: lir::TempId,
    id: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
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

fn lower_let_bind<'s>(
    to: lir::TempId,
    local: ir::LocalId,
    value: ir::ExprId,
    in_: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::BlockId {
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
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
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
    to: lir::TempId,
    operand: ir::ExprId,
    arms: &[(ir::Immediate, ir::ExprId)],
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> Vec<lir::BlockId> {
    let operand_temp = compiler.new_temp();
    let block = lower_expr(
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
        ir::Immediate::Float(_) => unreachable!(),
    }

    branches_end.push(default_branch_end);
    branches_end
}

fn lower_cond<'s>(
    to: lir::TempId,
    arms: &[(ir::ExprId, ir::ExprId)],
    default: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> Vec<lir::BlockId> {
    let mut expr_map = HashMap::new();

    let mut branches_end = Vec::new();
    let block = arms.iter().fold(block, |block, (cond, e)| {
        let test = compiler.new_temp();
        let block = lower_expr(
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
    to: lir::TempId,
    test: ir::ExprId,
    then: ir::ExprId,
    else_: ir::ExprId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> Vec<lir::BlockId> {
    let test_temp = compiler.new_temp();
    let block = lower_expr(
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

fn lower_call<'s>(
    to: lir::TempId,
    callee: ir::ExprId,
    args: &[ir::ExprId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
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
            let block = emit_arguments!(block);
            blocks[block].push(CallBuiltin(to, builtins.id_by_name(name), args_temp));
            block
        }
        // Optimization: Tail call of non-closure function
        ir::ExprNode::UserFunction(fid)
            if *fid == compiler.fid
                && args.len() == compiler.arity
                && to == compiler.ret_temp() =>
        {
            let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, functions);
            let block = emit_arguments!(block);
            blocks[block].push(TailCallU(to, lir_fid, args_temp));
            block
        }
        // Optimization: we don't load a user function and then apply arguments to it if we have enough arguments.
        // Instead, we call this user function directly
        ir::ExprNode::UserFunction(name) if args.len() == ir_functions.get(name).unwrap().arity => {
            let block = emit_arguments!(block);
            let lir_fid = lower_function(name, ir_nodes, ir_functions, builtins, functions);
            blocks[block].push(Call(to, lir_fid, args_temp));
            block
        }
        // Optimization: Tall call of closure
        ir::ExprNode::ThisClosure(local)
            if args.len() == compiler.arity && to == compiler.ret_temp() =>
        {
            let callee_temp = compiler.local_id_to_temp(*local);
            let block = emit_arguments!(block);
            blocks[block].push(TailCall(to, callee_temp, args_temp));
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
            let block = emit_arguments!(block);
            blocks[block].push(Apply(to, callee_temp, args_temp));
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
    };

    blocks[block].push(inst);
    block
}

fn lower_make_closure<'s>(
    to: lir::TempId,
    fid: &ir::FunctionId,
    captures: &[ir::LocalId],
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, functions);
    let temps: Vec<_> = captures
        .iter()
        .map(|local| compiler.local_id_to_temp(*local))
        .collect();

    use lir::Instruction::*;
    let f_temp = compiler.new_temp();
    blocks[block].push(Function(f_temp, lir_fid));
    blocks[block].push(Push(to, f_temp, temps));
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
    fid: &ir::FunctionId,
    ir_nodes: &ir::ExprHeap<'s>,
    ir_functions: &ir::FunctionTable,
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
) -> lir::BlockId {
    let lir_fid = lower_function(fid, ir_nodes, ir_functions, builtins, functions);
    blocks[block].push(lir::Instruction::Function(to, lir_fid));
    block
}

fn lower_builtin<'s>(
    to: lir::TempId,
    fid: &ir::FunctionId,
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
) -> lir::BlockId {
    let builtin_id = builtins.id_by_name(fid);
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
    builtins: &lir::Builtins,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
    functions: &mut lir::FunctionTable,
    compiler: &mut Compiler,
) -> lir::BlockId {
    let fields_temp: Vec<_> = fields.iter().map(|_| compiler.new_temp()).collect();
    let block = fields
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

    blocks[block].push(lir::Instruction::Tagged(to, tag_code, fields_temp));
    block
}

fn lower_panic<'s>(
    sid: ir::StrId,
    block: lir::BlockId,
    blocks: &mut lir::BlockHeap,
) -> lir::BlockId {
    blocks.seal(block, lir::Jump::Panic(sid));
    block
}
