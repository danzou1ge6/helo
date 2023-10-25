use crate::ir;
use pretty::{self};

pub fn pretty_ir_function<'s, 'b, D, A>(
    f: &ir::Function,
    name: &str,
    nodes: &ir::ExprHeap<'s>,
    str_list: &ir::StrList,
    allocator: &'b D,
) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    allocator
        .text("fn ")
        .append(allocator.text(name.to_string()))
        .append(format!(" ({})", f.arity))
        .append(allocator.text(" ="))
        .append(allocator.hardline())
        .append(pretty_ir(f.body, nodes, str_list, allocator).indent(2))
}

pub fn pretty_ir<'s, 'b, D, A>(
    id: ir::ExprId,
    nodes: &ir::ExprHeap<'s>,
    str_list: &ir::StrList,
    allocator: &'b D,
) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    use ir::ExprNode::*;
    match nodes[id].node() {
        LetBind { local, value, in_ } => allocator
            .text("LET")
            .append(allocator.softline())
            .append(
                allocator
                    .text(local.to_string())
                    .append(allocator.text(" = "))
                    .append(pretty_ir(*value, nodes, str_list, allocator))
                    .indent(2),
            )
            .append(allocator.space())
            .append("IN")
            .append(allocator.hardline())
            .append(pretty_ir(*in_, nodes, str_list, allocator))
            .align(),
        SwitchTag(operand, v, default) => allocator
            .text("SWITCH_TAG ")
            .append(pretty_ir(*operand, nodes, str_list, allocator))
            .append(allocator.hardline())
            .append(
                allocator
                    .intersperse(
                        v.iter().map(|(tag, e)| {
                            allocator
                                .text("| ")
                                .append(allocator.text(tag.name().to_string()))
                                .append(" -> ")
                                .append(pretty_ir(*e, nodes, str_list, allocator))
                        }),
                        allocator.hardline(),
                    )
                    .align()
                    .indent(2),
            )
            .append(allocator.hardline())
            .append(
                allocator
                    .text("| _ -> ")
                    .append(pretty_ir(*default, nodes, str_list, allocator))
                    .indent(2),
            ),
        Switch(operand, v, default) => allocator
            .text("SWITCH ")
            .append(pretty_ir(*operand, nodes, str_list, allocator))
            .append(allocator.hardline())
            .append(
                allocator
                    .intersperse(
                        v.iter().map(|(immediate, e)| {
                            allocator
                                .text("| ")
                                .append(allocator.text(immediate.to_string(str_list)))
                                .append(" -> ")
                                .append(pretty_ir(*e, nodes, str_list, allocator))
                        }),
                        allocator.hardline(),
                    )
                    .align()
                    .indent(2),
            )
            .append(allocator.hardline())
            .append(
                allocator
                    .text("| _ -> ")
                    .append(pretty_ir(*default, nodes, str_list, allocator))
                    .indent(2),
            ),
        Cond(v, default) => allocator.text("COND").append(allocator.softline()).append(
            allocator
                .nil()
                .append(
                    allocator
                        .intersperse(
                            v.iter().map(|(case, e)| {
                                allocator
                                    .text("if ")
                                    .append(pretty_ir(*case, nodes, str_list, allocator))
                                    .append(" -> ")
                                    .append(pretty_ir(*e, nodes, str_list, allocator))
                            }),
                            allocator.hardline(),
                        )
                        .indent(2),
                )
                .append(allocator.hardline())
                .append(
                    allocator
                        .text("if true -> ")
                        .append(pretty_ir(*default, nodes, str_list, allocator))
                        .indent(2),
                )
                .align(),
        ),
        IfElse { test, then, else_ } => allocator
            .text("IF ")
            .append(pretty_ir(*test, nodes, str_list, allocator))
            .append(allocator.hardline())
            .append("THEN")
            .append(
                allocator
                    .softline()
                    .append(pretty_ir(*then, nodes, str_list, allocator))
                    .indent(2),
            )
            .append(allocator.hardline())
            .append("ELSE")
            .append(
                allocator
                    .softline()
                    .append(pretty_ir(*else_, nodes, str_list, allocator))
                    .indent(2),
            ),
        Call { callee, args } => allocator
            .text("(")
            .append(pretty_ir(*callee, nodes, str_list, allocator))
            .append(allocator.softline())
            .append(
                allocator
                    .intersperse(
                        args.iter()
                            .map(|arg| pretty_ir(*arg, nodes, str_list, allocator)),
                        allocator.softline(),
                    )
                    .indent(2)
                    .group(),
            )
            .append(")")
            .align(),
        Immediate(im) => allocator.text(im.to_string(str_list)),
        MakeClosure(f, captures) => allocator
            .text("(MAKE_CLOSURE ")
            .append(allocator.text(f.clone()).indent(2))
            .append(allocator.space())
            .append(
                allocator
                    .intersperse(
                        captures.iter().map(|cap| allocator.text(cap.to_string())),
                        allocator.softline(),
                    )
                    .indent(2)
                    .group(),
            )
            .append(")"),
        Local(local) => allocator.text(local.to_string()),
        UserFunction(f) => allocator.text(f.clone()),
        Builtin(builtin) => allocator.text(builtin.clone()),
        VariantField(local, field) | TupleField(local, field) => {
            allocator.text(format!("{}.{}", local, field))
        }
        MakeTuple(elems) => allocator
            .text("(MAKE_TUPLE")
            .append(
                allocator
                    .intersperse(
                        elems
                            .iter()
                            .map(|elem| pretty_ir(*elem, nodes, str_list, allocator)),
                        allocator.softline(),
                    )
                    .indent(2)
                    .group(),
            )
            .append(")"),
        MakeTagged(tag, elems) => allocator
            .text("(MAKE_TAGGED")
            .append(allocator.softline())
            .append(allocator.text(tag.name().to_string()).indent(2))
            .append(allocator.softline())
            .append(
                allocator
                    .intersperse(
                        elems
                            .iter()
                            .map(|elem| pretty_ir(*elem, nodes, str_list, allocator)),
                        allocator.softline(),
                    )
                    .indent(2)
                    .group(),
            )
            .append(")"),
        ThisClosure(_) => allocator.text("THIS_CLOSURE"),
        Panic(msg) => allocator
            .text("(PANIC")
            .append(allocator.softline())
            .append(allocator.text(str_list.get(*msg).to_string()).indent(2))
            .append(allocator.text(")"))
            .align(),
    }
}

use crate::lir;

pub fn pretty_lir_instruction<'s, 'b, D, A>(
    inst: &lir::Instruction,
    str_list: &ir::StrList,
    functions: &lir::FunctionNameList,
    allocator: &'b D,
) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    use lir::Instruction::*;
    match inst {
        Apply(ret, callee, args) => allocator
            .text(format!("x{:<3} <- APPLY x{} ", ret, callee))
            .append(
                allocator.intersperse(args.iter().map(|r| format!("x{}", r)), allocator.text(",")),
            ),
        CallThisClosure(ret, callee, args) => allocator
            .text(format!("x{:<3} <- CALL_THIS_CLOSURE x{} ", ret, callee))
            .append(
                allocator.intersperse(args.iter().map(|r| format!("x{}", r)), allocator.text(",")),
            ),
        Call(ret, callee, args) => {
            let callee_name = functions.get(*callee);
            let callee_name = str_list.get(callee_name);
            allocator
                .text(format!("x{:<3} <- CALL f{}'{}' ", ret, callee, callee_name))
                .append(
                    allocator
                        .intersperse(args.iter().map(|r| format!("x{}", r)), allocator.text(",")),
                )
        }
        CallBuiltin(ret, callee, args) => {
            let callee_name = helo_runtime::builtins::name_by_id(*callee);
            allocator
                .text(format!(
                    "x{:<3} <- CALL_BUILTIN B{}'{}' ",
                    ret, callee, callee_name
                ))
                .append(
                    allocator
                        .intersperse(args.iter().map(|r| format!("x{}", r)), allocator.text(",")),
                )
        }
        Int(ret, value) => allocator.text(format!("x{:<3} <- {}", ret, value)),
        Bool(ret, value) => allocator.text(format!("x{:<3} <- {}", ret, value)),
        Float(ret, value) => allocator.text(format!("x{:<3} <- {}", ret, value)),
        Str(ret, value) => {
            let s = str_list.get(*value);
            allocator.text(format!("x{:<3} <- s{}'{}'", ret, value, s))
        }
        Char(ret, value) => allocator.text(format!("x{:<3} <- '{}'", ret, value)),
        AddToEnv(to, fid, args) => allocator.text(format!("x{:<3} <- ADD_TO_ENV f{} ", to, fid)).append(
            allocator.intersperse(args.iter().map(|r| format!("x{}", r)), allocator.text(",")),
        ),
        Push(to, operand, args) => allocator
            .text(format!("x{:<3} <- PUSH x{} ", to, operand))
            .append(
                allocator.intersperse(args.iter().map(|r| format!("x{}", r)), allocator.text(",")),
            ),
        Function(ret, value) => {
            let callee_name = functions.get(*value);
            let callee_name = str_list.get(callee_name);
            allocator.text(format!("x{:<3} <- f{}'{}'", ret, value, callee_name))
        }
        Buitltin(ret, value) => {
            let callee_name = helo_runtime::builtins::name_by_id(*value);
            allocator.text(format!("x{:<3} <- f{}'{}'", ret, value, callee_name))
        }
        Field(ret, operand, n) => allocator.text(format!("x{:<3} <- x{}.{}", ret, operand, n)),
        Tagged(to, tag, args) => allocator
            .text(format!("x{:<3} <- TAGGED {} ", to, tag))
            .append(
                allocator.intersperse(args.iter().map(|r| format!("x{}", r)), allocator.text(",")),
            ),
        Mov(to, from) => allocator.text(format!("x{:<3} <- x{}", to, from)),
    }
}

pub fn pretty_lir_jump<'s, 'b, D, A>(
    jump: &lir::Jump,
    str_list: &ir::StrList,
    allocator: &'b D,
) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    use lir::Jump::*;
    match jump {
        JumpTable(t, tab) => allocator
            .text(format!("JUMP_TABLE x{}", t))
            .append(allocator.space())
            .append(
                allocator
                    .intersperse(
                        tab.iter().map(|b| allocator.text(format!("b{}", *b))),
                        allocator.hardline(),
                    )
                    .align()
                    .indent(4),
            ),
        JumpIfElse(test, then_, else_) => {
            allocator.text(format!("JUMP_IF_ELSE x{} b{} b{}", test, then_, else_))
        }
        JumpSwitchInt(t, arms, default) => allocator
            .text(format!("JUMP_SWITCH_INT x{}", t))
            .append(allocator.space())
            .append(
                allocator
                    .intersperse(
                        arms.iter()
                            .map(|(value, b)| allocator.text(format!("{} => b{}", *value, *b)))
                            .chain([allocator.text(format!("_ => {}", *default))].into_iter()),
                        allocator.hardline(),
                    )
                    .align()
                    .indent(4),
            ),
        JumpSwitchChar(t, arms, default) => allocator
            .text(format!("JUMP_SWITCH_CHAR x{}", t))
            .append(allocator.space())
            .append(
                allocator
                    .intersperse(
                        arms.iter()
                            .map(|(value, b)| allocator.text(format!("{} => b{}", *value, *b)))
                            .chain([allocator.text(format!("_ => {}", *default))].into_iter()),
                        allocator.hardline(),
                    )
                    .align()
                    .indent(4),
            ),
        JumpSwitchStr(t, arms, default) => allocator
            .text(format!("JUMP_SWITCH_STR x{}", t))
            .append(allocator.space())
            .append(
                allocator
                    .intersperse(
                        arms.iter()
                            .map(|(value, b)| {
                                allocator.text(format!(
                                    "{}'{}' => b{}",
                                    *value,
                                    str_list.get(*value),
                                    *b
                                ))
                            })
                            .chain([allocator.text(format!("_ => {}", *default))].into_iter()),
                        allocator.hardline(),
                    )
                    .align()
                    .indent(4),
            ),
        Jump(to) => allocator.text(format!("JUMP b{}", to)),
        Panic(value) => {
            let s = str_list.get(*value);
            allocator.text(format!("PANIC s{}'{}'", value, s))
        }
        Ret(t) => allocator.text(format!("RET x{}", t)),
    }
}

pub fn pretty_lir_block<'s, 'b, D, A>(
    block_id: lir::BlockId,
    blocks: &lir::BlockHeap,
    str_list: &ir::StrList,
    functions: &lir::FunctionNameList,
    allocator: &'b D,
) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    allocator
        .text(format!("Block {} [Predecessors ", block_id))
        .append(allocator.intersperse(
            blocks[block_id].predessorss().map(|b| format!("b{}", b)),
            allocator.text(","),
        ))
        .append(allocator.text("]:"))
        .append(allocator.hardline())
        .append(
            allocator
                .intersperse(
                    blocks[block_id]
                        .iter()
                        .map(|inst| pretty_lir_instruction(inst, str_list, functions, allocator)),
                    allocator.hardline(),
                )
                .align()
                .indent(4),
        )
        .append(allocator.hardline())
        .append(pretty_lir_jump(blocks[block_id].exit(), str_list, allocator).indent(4))
}

pub fn pretty_ssa_block<'s, 'b, D, A>(
    block_id: lir::BlockId,
    blocks: &lir::ssa::SsaBlockHeap,
    str_list: &ir::StrList,
    functions: &lir::FunctionNameList,
    allocator: &'b D,
) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    allocator
        .text(format!("Block {} [Predecessors ", block_id))
        .append(allocator.intersperse(
            blocks[block_id].pred.iter().map(|b| format!("b{}", b)),
            allocator.text(","),
        ))
        .append(allocator.text("]:"))
        .append(if blocks[block_id].phis.len() != 0 {
            allocator.hardline()
        } else {
            allocator.nil()
        })
        .append(
            allocator
                .intersperse(
                    blocks[block_id]
                        .phis
                        .iter()
                        .map(|lir::ssa::Phi(ret, args)| {
                            allocator.text(format!("x{:<3} <- PHI ", ret)).append(
                                allocator.intersperse(
                                    args.iter().map(|arg| allocator.text(format!("x{}", arg))),
                                    allocator.text(","),
                                ),
                            )
                        }),
                    allocator.hardline(),
                )
                .align()
                .indent(4),
        )
        .append(if blocks[block_id].body.len() != 0 {
            allocator.hardline()
        } else {
            allocator.nil()
        })
        .append(
            allocator
                .intersperse(
                    blocks[block_id]
                        .body
                        .iter()
                        .map(|inst| pretty_lir_instruction(inst, str_list, functions, allocator)),
                    allocator.hardline(),
                )
                .align()
                .indent(4),
        )
        .append(if blocks[block_id].tail_copies.len() != 0 {
            allocator
                .hardline()
                .append(allocator.text("COPY").indent(4))
        } else {
            allocator.nil()
        })
        .append(
            allocator.intersperse(
                blocks[block_id]
                    .tail_copies
                    .iter()
                    .map(|(from, to)| allocator.text(format!("x{} -> x{}", from, to))),
                allocator.text(", "),
            ),
        )
        .append(allocator.hardline())
        .append(pretty_lir_jump(&blocks[block_id].exit, str_list, allocator).indent(4))
}

pub fn pretty_ssa_function<'s, 'b, D, A>(
    f_id: lir::FunctionId,
    f: &lir::ssa::Function,
    str_list: &ir::StrList,
    functions: &lir::FunctionNameList,
    allocator: &'b D,
) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    allocator
        .text(format!(
            "Function {}'{}' [Entry at Block {}, Arity {}]:",
            f_id,
            str_list.get(f.name),
            f.body,
            f.arity
        ))
        .append(allocator.hardline())
        .append(
            allocator
                .intersperse(
                    f.blocks
                        .iter_id()
                        .map(|b| pretty_ssa_block(b, &f.blocks, str_list, functions, allocator)),
                    allocator.hardline(),
                )
                .align()
                .indent(4),
        )
}

pub fn pretty_lir_function<'s, 'b, D, A>(
    f_id: lir::FunctionId,
    f: &lir::Function,
    str_list: &ir::StrList,
    functions_names: &lir::FunctionNameList,
    allocator: &'b D,
) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    allocator
        .text(format!(
            "Function {}'{}' [Entry at Block {}, Arity {}]:",
            f_id,
            str_list.get(f.name),
            f.body,
            f.arity
        ))
        .append(allocator.hardline())
        .append(
            allocator
                .intersperse(
                    f.blocks.iter_id().map(|b| {
                        pretty_lir_block(b, &f.blocks, str_list, functions_names, allocator)
                    }),
                    allocator.hardline(),
                )
                .align()
                .indent(4),
        )
}
