use crate::ir;
use pretty::{self};

pub fn pretty_function<'s, 'b, D, A>(
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
        .append(pretty_ir(f.body, nodes, str_list, allocator).nest(2))
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
                    .nest(2),
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
                    .nest(2),
            )
            .append(allocator.hardline())
            .append(
                allocator
                    .text("| _ -> ")
                    .append(pretty_ir(*default, nodes, str_list, allocator))
                    .nest(2),
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
                    .nest(2),
            )
            .append(allocator.hardline())
            .append(
                allocator
                    .text("| _ -> ")
                    .append(pretty_ir(*default, nodes, str_list, allocator))
                    .nest(2),
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
                        .nest(2),
                )
                .append(allocator.hardline())
                .append(
                    allocator
                        .text("if true -> ")
                        .append(pretty_ir(*default, nodes, str_list, allocator))
                        .nest(2),
                )
                .align(),
        ),
        IfElse { test, then, else_ } => allocator
            .text("IF ")
            .append(pretty_ir(*test, nodes, str_list, allocator))
            .append(allocator.softline())
            .append("THEN")
            .append(
                allocator
                    .softline()
                    .append(pretty_ir(*then, nodes, str_list, allocator))
                    .nest(2),
            )
            .append(allocator.softline())
            .append("ELSE")
            .append(
                allocator
                    .softline()
                    .append(pretty_ir(*else_, nodes, str_list, allocator))
                    .nest(2),
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
                    .nest(2)
                    .group(),
            )
            .append(")")
            .align(),
        Immediate(im) => allocator.text(im.to_string(str_list)),
        MakeClosure(f, captures) => allocator
            .text("(MAKE_CLOSURE ")
            .append(allocator.text(f.clone()).nest(2))
            .append(allocator.space())
            .append(
                allocator
                    .intersperse(
                        captures.iter().map(|cap| allocator.text(cap.to_string())),
                        allocator.softline(),
                    )
                    .nest(2)
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
                    .nest(2)
                    .group(),
            )
            .append(")"),
        MakeTagged(tag, elems) => allocator
            .text("(MAKE_TAGGED")
            .append(allocator.softline())
            .append(allocator.text(tag.name().to_string()).nest(2))
            .append(allocator.softline())
            .append(
                allocator
                    .intersperse(
                        elems
                            .iter()
                            .map(|elem| pretty_ir(*elem, nodes, str_list, allocator)),
                        allocator.softline(),
                    )
                    .nest(2)
                    .group(),
            )
            .append(")"),
        ThisClosure(f) => allocator.text(f.clone()),
        Panic(msg) => allocator
            .text("(PANIC")
            .append(allocator.softline())
            .append(allocator.text(str_list.get(*msg).to_string()).nest(2))
            .append(allocator.text(")"))
            .align(),
    }
}
