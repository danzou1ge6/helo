use super::*;

fn expr_size<'s>(expr: ExprId, nodes: &ExprHeap<'s>) -> usize {
    use ExprNode::*;
    match nodes[expr].node() {
        Never => 0,
        LetBind { value, in_, .. } => 1 + expr_size(*value, nodes) + expr_size(*in_, nodes),
        SwitchTag(value, arms, default) => {
            1 + expr_size(*value, nodes)
                + arms
                    .iter()
                    .map(|(_, e)| expr_size(*e, nodes))
                    .max()
                    .unwrap_or(0)
                    .max(expr_size(*default, nodes))
        }
        Switch(value, arms, default) => {
            1 + expr_size(*value, nodes)
                + arms
                    .iter()
                    .map(|(_, e)| expr_size(*e, nodes))
                    .max()
                    .unwrap_or(0)
                    .max(expr_size(*default, nodes))
        }
        Cond(arms, default) => {
            1 + arms
                .iter()
                .map(|(test, e)| expr_size(*test, nodes) + expr_size(*e, nodes))
                .max()
                .unwrap_or(0)
                .max(expr_size(*default, nodes))
        }
        IfElse { test, then, else_ } => {
            1 + expr_size(*test, nodes) + expr_size(*then, nodes).max(expr_size(*else_, nodes))
        }
        Apply { callee, args, .. } => {
            1 + expr_size(*callee, nodes) + args.iter().map(|e| expr_size(*e, nodes)).sum::<usize>()
        }
        Immediate(..) | MakeClosure(..) | Local(..) | ThisClosure(..) | UserFunction(..)
        | Builtin(..) | VariantField(..) | TupleField(..) => 1,
        MakeTuple(v) | MakeTagged(_, v) => {
            1 + v.iter().map(|e| expr_size(*e, nodes)).sum::<usize>()
        }
        Panic { .. } => 0,
        Assign(_, e) => 1 + expr_size(*e, nodes),
        Seq(v, e) => {
            v.iter().map(|e| expr_size(*e, nodes)).sum::<usize>()
                + e.map(|e| expr_size(e, nodes)).unwrap_or(0)
        }
        If { test, then } => 1 + expr_size(*test, nodes) + expr_size(*then, nodes),
        While { test, then } => 1 + expr_size(*test, nodes) + expr_size(*then, nodes),
    }
}

fn contains_apply_to<'s>(expr: ExprId, nodes: &ExprHeap<'s>, f: &FunctionId<'s>) -> bool {
    let mut r = false;
    nodes.walk(expr, &mut |e| match e.node() {
        ExprNode::UserFunction(f1) => {
            if f1 == f {
                r = true
            }
        }
        _ => {}
    });
    r
}

struct InlineContext {
    local_offset: usize,
}

impl InlineContext {
    pub fn get_new_local(&mut self, old: LocalId) -> LocalId {
        old.offset(self.local_offset)
    }
    pub fn rewrite<'s>(&mut self, expr: ExprId, nodes: &mut ExprHeap<'s>) -> ExprId {
        nodes.apply(
            expr,
            &|e, nodes| {
                use ExprNode::*;
                match nodes[e].node() {
                    LetBind { .. } | Local(..) | VariantField(..) | TupleField(..) | MakeClosure(..) => true,
                    _ => false,
                }
            },
            &mut |e, nodes| {
                use ExprNode::*;
                let node = match nodes[e].node().clone() {
                    LetBind { local, value, in_ } => LetBind {
                        local: self.get_new_local(local),
                        value: self.rewrite(value, nodes),
                        in_: self.rewrite(in_, nodes),
                    },
                    Local(local) => Local(self.get_new_local(local)),
                    VariantField(local, i) => VariantField(self.get_new_local(local), i),
                    TupleField(local, i) => TupleField(self.get_new_local(local), i),
                    MakeClosure(fid, locals) => MakeClosure(
                        fid,
                        locals.into_iter().map(|i| self.get_new_local(i)).collect(),
                    ),
                    _ => unreachable!(),
                };

                nodes.push(Expr::new(node, nodes[e].meta().clone()))
            },
        )
    }
}

/// Rename locals in `i` and prepend it with `args` bound to locals that are parameters.
/// Need to know original number of locals from `old_local_cnt` in order to rename.
fn prepare_for_inline<'s>(
    old_local_cnt: usize,
    args: &[ExprId],
    i: ExprId,
    local_cnt: usize,
    nodes: &mut ExprHeap<'s>,
    call_site_meta: &Meta,
) -> (ExprId, usize) {
    let mut ctx = InlineContext {
        local_offset: old_local_cnt,
    };

    let new_local_cnt = old_local_cnt + local_cnt;

    let mut r = ctx.rewrite(i, nodes);
    for (k, arg) in args.iter().enumerate().rev() {
        let cloned_arg = nodes.push(nodes[*arg].clone());
        r = nodes.push(Expr::new(
            ExprNode::LetBind {
                local: LocalId(k + old_local_cnt),
                value: cloned_arg,
                in_: r,
            },
            call_site_meta.clone(),
        ));
    }

    (r, new_local_cnt)
}

fn replace_alias_for_function<'s>(
    f: &Function,
    aliases: &HashMap<FunctionId<'s>, AliasOf<'s>>,
    nodes: &mut ExprHeap<'s>,
) {
    nodes.apply_in_place(
        f.body,
        &|e, nodes| {
            use ExprNode::*;
            match nodes[e].node().clone() {
                Apply { callee, .. } => {
                    if let UserFunction(fid) = nodes[callee].node() {
                        return aliases.contains_key(fid);
                    }
                }
                _ => {}
            };
            false
        },
        &mut |e, nodes| {
            use ExprNode::*;
            let expr = match nodes[e].node().clone() {
                Apply {
                    callee,
                    args,
                    callee_impure,
                } => {
                    if let UserFunction(fid) = nodes[callee].node() {
                        // callee is alias
                        if let Some(alias_of) = aliases.get(fid) {
                            let callee_node = match alias_of {
                                AliasOf::Builtin(bid) => Builtin(bid.clone()),
                                AliasOf::User(fid) => UserFunction(fid.clone()),
                            };
                            let callee =
                                nodes.push(Expr::new(callee_node, nodes[callee].meta().clone()));
                            nodes.push(Expr::new(
                                Apply {
                                    callee,
                                    args,
                                    callee_impure,
                                },
                                nodes[e].meta().clone(),
                            ))
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    }
                }
                _ => unreachable!(),
            };

            expr
        },
    );
}

fn inline_for_function<'s>(
    mut f: Function,
    functions: &FunctionTable<'s>,
    can_inline: &impl Fn(&FunctionId<'s>) -> bool,
    nodes: &mut ExprHeap<'s>,
) -> Function {
    let mut f_local_cnt = f.local_cnt;
    nodes.apply_in_place(
        f.body,
        &|e, nodes| {
            use ExprNode::*;
            match nodes[e].node().clone() {
                Apply { callee, args, .. } => {
                    if let UserFunction(fid) = nodes[callee].node() {
                        return can_inline(fid) && args.len() == functions.get(fid).unwrap().arity;
                    }
                }
                _ => {}
            };
            false
        },
        &mut |e, nodes| {
            use ExprNode::*;
            let expr = match nodes[e].node().clone() {
                Apply { callee, args, .. } => {
                    if let UserFunction(fid) = nodes[callee].node() {
                        let f_inlined = functions.get(fid).unwrap();
                        // callee can be inlined
                        if can_inline(fid) && args.len() == f_inlined.arity {
                            let (body, new_local_cnt) = prepare_for_inline(
                                f_local_cnt,
                                &args,
                                f_inlined.body,
                                f_inlined.local_cnt,
                                nodes,
                                &nodes[e].meta().clone(),
                            );
                            f_local_cnt = new_local_cnt;
                            body
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    }
                }
                _ => unreachable!(),
            };

            expr
        },
    );
    f.local_cnt = f_local_cnt;
    f
}

enum AliasOf<'s> {
    Builtin(ast::BuiltinFunctionName<'s>),
    User(FunctionId<'s>),
}

fn is_alias_of<'s>(f: &Function, nodes: &ExprHeap<'s>) -> Option<AliasOf<'s>> {
    match nodes[f.body].node() {
        ExprNode::Apply { callee, args, .. } => {
            if args
                .iter()
                .enumerate()
                .all(|(i, e)| match nodes[*e].node() {
                    ExprNode::Local(LocalId(i1)) if *i1 == i => true,
                    _ => false,
                })
            {
                match nodes[*callee].node() {
                    ExprNode::UserFunction(fid) => Some(AliasOf::User(fid.clone())),
                    ExprNode::Builtin(bid) => Some(AliasOf::Builtin(bid.clone())),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn inline_ir<'s: 'a, 'a>(
    functions: FunctionTable<'s>,
    nodes: &mut ExprHeap<'s>,
    size_threshold: usize,
    entries: impl Iterator<Item = &'a ast::FunctionId<'s>>,
) -> FunctionTable<'s> {
    let entries = entries
        .map(|id| FunctionId::of_non_generic(id.clone()))
        .collect::<Vec<_>>();
    let sizes = functions
        .function_ids()
        .filter(|id| !id.ast_id.is_closure())
        .filter_map(|id| {
            let f = functions.get(id).unwrap();
            if contains_apply_to(f.body, nodes, id) {
                None
            } else {
                Some((id.clone(), expr_size(f.body, nodes)))
            }
        })
        .collect::<HashMap<_, _>>();
    let aliases = functions
        .iter()
        .filter_map(|(fid, f)| Some((fid.clone(), is_alias_of(f, &nodes)?)))
        .collect::<HashMap<_, _>>();

    // Aliasing
    for (_, f) in functions.iter() {
        replace_alias_for_function(f, &aliases, nodes);
    }

    // Inlining
    let mut new_functions = FunctionTable::new();

    let can_inline = |fid: &FunctionId<'s>| {
        sizes.get(fid).is_some_and(|s| *s < size_threshold) && !entries.contains(fid)
    };
    for (id, f) in functions.iter() {
        let new_f = if can_inline(id) {
            // inlineing modifies function body in place, so functions that are inlined
            // cannot be modified
            f.clone()
        } else {
            inline_for_function(f.clone(), &functions, &can_inline, nodes)
        };
        new_functions.insert(id.clone(), new_f);
    }

    new_functions
}
