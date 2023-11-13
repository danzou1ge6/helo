use crate::ast;
use crate::errors;
use crate::inferer;
use crate::typed;
use errors::ManyErrorReceive;
use std::collections::HashMap;
use std::collections::HashSet;

fn infer_expr<'s>(
    expr_id: ast::ExprId,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let expr = &ast_nodes[expr_id];
    use ast::ExprNode;
    let typed_expr = match &expr.node {
        ExprNode::Apply { callee, args } => infer_call(
            *callee,
            args,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::IfElse { test, then, else_ } => infer_if_else(
            *test,
            *then,
            *else_,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::Case { operand, arms } => infer_case(
            *operand,
            arms,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::LetIn { bind, value, in_ } => infer_let_in(
            *bind,
            *value,
            *in_,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::LetPatIn { bind, value, in_ } => infer_let_pattern_in(
            bind,
            *value,
            *in_,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::Captured {
            id,
            is_self,
            mutable,
        } => infer_captured(
            *id,
            *is_self,
            *mutable,
            &expr.meta,
            symbols,
            typed_functions,
        ),
        ExprNode::MakeClosure(closure) => infer_make_closure(
            closure,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::Global(global) => infer_global(
            &global,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::Tuple(v) => infer_tuple(
            v,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::Constant(constant) => infer_constant(constant, &expr.meta),
        ExprNode::Local(id, mutable) => infer_local(*id, *mutable, &expr.meta),
        ExprNode::Seq(stmts, result) => infer_seq(
            stmts.iter(),
            *result,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::Assign(to, from) => infer_assign(
            *to,
            *from,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::Unit => {
            e.push(errors::NoUnitHere::new(&expr.meta));
            typed::Expr {
                node: typed::ExprNode::Never,
                type_: ast::Type::new_never(),
                meta: expr.meta.clone(),
            }
        }
    };
    if let Some((type_annotated, annotation_meta)) = &expr.type_ {
        let provided_type = inferer.instantiate_wildcard(type_annotated);
        inferer
            .unify(&provided_type, &typed_expr.type_)
            .map_err(|_| {
                errors::UnificationFailure::new(
                    &provided_type,
                    annotation_meta,
                    &typed_expr.type_,
                    &expr.meta,
                    &expr.meta,
                )
            })
            .commit(e);
    }

    typed_expr
}

fn infer_stmt<'s>(
    stmt: &ast::Stmt,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Stmt {
    match &stmt.node {
        ast::StmtNode::If { test, then } => {
            let test = infer_expr(
                *test,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                e,
            );
            let then = infer_expr(
                *then,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                e,
            );
            let test = typed_nodes.push(test);
            let then = typed_nodes.push(then);
            typed::Stmt::new(typed::StmtNode::If { test, then }, stmt.meta.clone())
        }
        ast::StmtNode::While { test, then } => {
            let test = infer_expr(
                *test,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                e,
            );
            let then = infer_expr(
                *then,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                e,
            );
            let test = typed_nodes.push(test);
            let then = typed_nodes.push(then);
            typed::Stmt::new(typed::StmtNode::While { test, then }, stmt.meta.clone())
        }
        ast::StmtNode::Expr(expr) => {
            let expr = infer_expr(
                *expr,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                e,
            );
            let expr = typed_nodes.push(expr);
            typed::Stmt::new(typed::StmtNode::Expr(expr), stmt.meta.clone())
        }
    }
}

fn infer_seq<'s, 'a>(
    stmts: impl Iterator<Item = &'a ast::Stmt>,
    result: Option<ast::ExprId>,
    stmts_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let stmts = stmts
        .map(|stmt| {
            infer_stmt(
                stmt,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                e,
            )
        })
        .collect();

    let result = result.map(|id| {
        infer_expr(
            id,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        )
    });

    let type_ = result.as_ref().map_or(
        ast::Type {
            node: ast::TypeNode::Unit,
        },
        |expr| expr.type_.clone(),
    );

    let result = result.map(|r| typed_nodes.push(r));

    typed::Expr {
        node: typed::ExprNode::Seq(stmts, result),
        type_,
        meta: stmts_meta.clone(),
    }
}

fn infer_assign<'s>(
    to: ast::ExprId,
    from: ast::ExprId,
    assign_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let from = infer_expr(
        from,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );
    match &ast_nodes[to].node {
        ast::ExprNode::Local(local, true) => typed::Expr {
            node: typed::ExprNode::AssignLocal(*local, typed_nodes.push(from)),
            type_: ast::Type::new_unit(),
            meta: assign_meta.clone(),
        },
        ast::ExprNode::Captured {
            id, mutable: true, ..
        } => typed::Expr {
            node: typed::ExprNode::AssignCaptured(*id, typed_nodes.push(from)),
            type_: ast::Type::new_unit(),
            meta: assign_meta.clone(),
        },
        _ => {
            e.push(errors::OnlyLocalAssign::new(assign_meta));
            typed::Expr {
                node: typed::ExprNode::Never,
                type_: ast::Type::new_never(),
                meta: assign_meta.clone(),
            }
        }
    }
}

fn infer_expr_many<'s, 'a>(
    expr_ids: impl Iterator<Item = &'a ast::ExprId>,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> Vec<typed::Expr<'s>> {
    expr_ids
        .map(|i| {
            infer_expr(
                *i,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                e,
            )
        })
        .collect()
}

fn infer_tuple<'s>(
    elements: &[ast::ExprId],
    tuple_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let elements = infer_expr_many(
        elements.into_iter(),
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );
    let type_ = elements.iter().map(|e| e.type_.clone()).collect();
    let elements = typed_nodes.push_many(elements.into_iter());
    typed::Expr {
        node: typed::ExprNode::Tuple(elements),
        type_: ast::Type {
            node: ast::TypeNode::Tuple(type_),
        },
        meta: tuple_meta.clone(),
    }
}

fn infer_call<'s>(
    callee: ast::ExprId,
    args: &[ast::ExprId],
    call_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let callee = infer_expr(
        callee,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );
    let args = infer_expr_many(
        args.iter(),
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );

    let callee_type_node = inferer
        .resolve(&callee.type_, &callee.meta)
        .unwrap_or_else(|err| {
            e.push(err);
            ast::Type::new_never()
        })
        .node;
    let ret_type = match &callee_type_node {
        // NOTE that we assume that type-vars have already been renamed
        ast::TypeNode::Callable(ast::CallableType { params, ret, .. })
        | ast::TypeNode::ImpureCallable(ast::CallableType { params, ret }) => {
            if args.len() > params.len() {
                e.push(errors::TooManyArguments::new(call_meta, params.len()));
            }
            inferer
                .unify_list(args.iter().map(|a| &a.type_), params.iter())
                .map_err(|i| {
                    errors::ArgumentUnificationFailure::new(
                        &callee.type_,
                        &callee.meta,
                        &args[i].type_,
                        &args[i].meta,
                        call_meta,
                    )
                })
                .commit(e);

            if callee_type_node.impure()
                && symbols
                    .function(typed_functions.currently_infering().unwrap())
                    .pure
            {
                e.push(errors::InpureFunctionInPureFunction::new(call_meta));
            }

            let type_constructor = if callee_type_node.impure() {
                ast::TypeNode::ImpureCallable
            } else {
                ast::TypeNode::Callable
            };

            if args.len() >= params.len() {
                ret.as_ref().clone()
            } else {
                // Curried
                let ret_callable_type = ast::CallableType {
                    params: params[args.len()..params.len()].to_vec(),
                    ret: ret.clone(),
                };
                let ret_type = ast::Type {
                    node: type_constructor(ret_callable_type),
                };
                ret_type
            }
        }
        ast::TypeNode::Var(v_id) => {
            let ret_type = ast::Type {
                node: ast::TypeNode::Var(inferer.alloc_var()),
            };

            let type_constructor = if symbols
                .function(typed_functions.currently_infering().unwrap())
                .pure
            {
                ast::TypeNode::Callable
            } else {
                ast::TypeNode::ImpureCallable
            };

            inferer
                .update_var(
                    *v_id,
                    &ast::Type {
                        node: type_constructor(ast::CallableType {
                            params: args.iter().map(|a| a.type_.clone()).collect(),
                            ret: Box::new(ret_type.clone()),
                        }),
                    },
                )
                .map_err(|_| {
                    errors::ArgumentsUnificationFailure::new(
                        &callee.type_,
                        args.iter().map(|arg| &arg.type_),
                        call_meta,
                    )
                })
                .commit(e);
            ret_type
        }
        otherwise => {
            if !matches!(otherwise, ast::TypeNode::Never) {
                e.push(errors::NotCallable::new(&callee.type_, &callee.meta));
            }
            ast::Type::new_never()
        }
    };

    typed::Expr {
        node: typed::ExprNode::Apply {
            callee: typed_nodes.push(callee),
            args: typed_nodes.push_many(args.into_iter()),
        },
        type_: ret_type,
        meta: call_meta.clone(),
    }
}

fn infer_let_in<'s>(
    bind: ast::LocalId,
    value: ast::ExprId,
    in_: ast::ExprId,
    let_in_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let value = infer_expr(
        value,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );
    // Bind type of value to local
    inferer
        .update_var(type_var_id_for_local(bind), &value.type_)
        .map_err(|_| {
            errors::LocalUnificationFailure::new(bind, &value.type_, &value.meta, let_in_meta)
        })
        .commit(e);

    // Infer type of in clause
    let in_ = infer_expr(
        in_,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );

    let ret_type = in_.type_.clone();
    typed::Expr {
        node: typed::ExprNode::LetIn {
            bind,
            value: typed_nodes.push(value),
            in_: typed_nodes.push(in_),
        },
        type_: ret_type,
        meta: let_in_meta.clone(),
    }
}

fn infer_let_pattern_in<'s>(
    pattern: &ast::Pattern<'s>,
    value: ast::ExprId,
    in_: ast::ExprId,
    let_in_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    if !pattern.inrefutable(symbols) {
        e.push(errors::RefutablePattern::new(pattern.meta()));
    }

    let pattern_type = infer_pattern_type(pattern, symbols, inferer, e);
    let value = infer_expr(
        value,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );
    // Bind type of value to pattern
    inferer
        .unify(&pattern_type, &value.type_)
        .map_err(|_| {
            errors::UnificationFailure::new(
                &pattern_type,
                pattern.meta(),
                &value.type_,
                &value.meta,
                let_in_meta,
            )
        })
        .commit(e);

    // Infer type of in clause
    let in_ = infer_expr(
        in_,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );

    let ret_type = in_.type_.clone();
    typed::Expr {
        node: typed::ExprNode::LetPatIn {
            bind: pattern.clone(),
            value: typed_nodes.push(value),
            in_: typed_nodes.push(in_),
        },
        type_: ret_type,
        meta: let_in_meta.clone(),
    }
}

fn infer_global<'s>(
    name: &'s str,
    global_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    // Global is user-defined
    if let Some(f) = symbols.get_function(name) {
        if let Some(renamed_f_type) = infer_function_type_renamed(
            name,
            global_meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ) {
            let type_constructor = if f.pure {
                ast::TypeNode::Callable
            } else {
                ast::TypeNode::ImpureCallable
            };

            return typed::Expr {
                node: typed::ExprNode::UserFunction(name),
                type_: ast::Type {
                    node: type_constructor(renamed_f_type.into()),
                },
                meta: global_meta.clone(),
            };
        }
        // Fail to infer `name`'s signature, but that's not a error here
        // So we fail with never type silently

        // Global is builtin
    } else if let Some(f) = symbols.get_builtin(name) {
        let renamed_type = inferer.rename_type_vars(&f.type_, f.var_cnt);

        let type_constructor = if f.pure {
            ast::TypeNode::Callable
        } else {
            ast::TypeNode::ImpureCallable
        };

        let r = typed::Expr {
            node: typed::ExprNode::Builtin(name),
            type_: ast::Type {
                node: type_constructor(renamed_type),
            },
            meta: global_meta.clone(),
        };
        return r;

    // Global is constructor
    } else if let Some(constructor) = symbols.get_constructor(name) {
        let data = symbols.data(constructor.belongs_to);
        let ret_type = ast::Type {
            node: ast::TypeNode::Generic(
                constructor.belongs_to,
                (0..data.kind_arity)
                    .map(|i| ast::Type::new_var(i.into()))
                    .collect(),
            ),
        };
        let type_ = ast::CallableType {
            params: constructor.params.clone(),
            ret: Box::new(ret_type),
        };
        let type_ = inferer.rename_type_vars(&type_, data.kind_arity);

        let type_ = if type_.params.len() == 0 {
            *type_.ret
        } else {
            ast::Type {
                node: ast::TypeNode::Callable(type_),
            }
        };
        return typed::Expr {
            node: typed::ExprNode::Constructor(name),
            type_,
            meta: global_meta.clone(),
        };

    // Fail: global doesn't exist
    } else {
        e.push(errors::GlobalNotFound::new(name, global_meta));
    }

    // Fail: fallthrough
    typed::Expr {
        node: typed::ExprNode::Never,
        type_: ast::Type::new_never(),
        meta: global_meta.clone(),
    }
}

fn infer_if_else<'s>(
    test: ast::ExprId,
    then: ast::ExprId,
    else_: ast::ExprId,
    if_else_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let test = infer_expr(
        test,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );
    let then = infer_expr(
        then,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );
    let else_ = infer_expr(
        else_,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );

    // then and else clause must have same type
    let ret_type = inferer.unify(&then.type_, &else_.type_).map_or_else(
        |_| {
            e.push(errors::UnificationFailure::new(
                &then.type_,
                &then.meta,
                &else_.type_,
                &else_.meta,
                if_else_meta,
            ));
            ast::Type::new_never()
        },
        |_| then.type_.clone(),
    );

    // Test clause must be bool
    inferer
        .unify(
            &test.type_,
            &ast::Type {
                node: ast::TypeNode::Primitive(ast::PrimitiveType::Bool),
            },
        )
        .map_or_else(
            |_| {
                e.push(errors::NonBoolTest::new(
                    &test.type_,
                    &test.meta,
                    if_else_meta,
                ))
            },
            |x| x,
        );

    typed::Expr {
        node: typed::ExprNode::IfElse {
            test: typed_nodes.push(test),
            then: typed_nodes.push(then),
            else_: typed_nodes.push(else_),
        },
        type_: ret_type,
        meta: if_else_meta.clone(),
    }
}

fn infer_constant<'s>(constant: &ast::Constant<'s>, constant_meta: &ast::Meta) -> typed::Expr<'s> {
    typed::Expr {
        node: typed::ExprNode::Constant(constant.clone()),
        type_: ast::Type {
            node: ast::TypeNode::Primitive(constant.type_()),
        },
        meta: constant_meta.clone(),
    }
}

fn infer_make_closure<'s>(
    closure_id: &ast::FunctionId,
    closure_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    if !symbols.function(&closure_id).pure
        && symbols
            .function(typed_functions.currently_infering().unwrap())
            .pure
    {
        e.push(errors::InpureClosureInPureFunction::new(closure_meta));
    }

    let closure_type = infer_function_type_renamed(
        closure_id,
        closure_meta,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    )
    .map_or_else(
        || ast::Type::new_never(),
        |c| {
            let f = symbols.function(&closure_id);
            let type_constructor = if f.pure {
                ast::TypeNode::Callable
            } else {
                ast::TypeNode::ImpureCallable
            };
            c.captures
                .iter()
                .zip(f.captures.iter())
                .zip(f.captures_meta.iter())
                .for_each(|((ctype, local_id), local_meta)| {
                    inferer
                        .update_var(type_var_id_for_local(*local_id), ctype)
                        .map_err(|_| {
                            errors::LocalUnificationFailure::new(
                                *local_id,
                                &ctype,
                                &local_meta,
                                closure_meta,
                            )
                        })
                        .commit(e);
                });
            ast::Type {
                node: type_constructor(c.into()),
            }
        },
    );
    typed::Expr {
        node: typed::ExprNode::MakeClosure(closure_id.clone()),
        type_: closure_type,
        meta: closure_meta.clone(),
    }
}

fn infer_pattern_type<'s>(
    pattern: &ast::Pattern<'s>,
    symbols: &ast::Symbols<'s>,
    inferer: &mut crate::inferer::Inferer<'s>,
    e: &mut crate::errors::ManyError,
) -> ast::Type<'s> {
    match pattern {
        ast::Pattern::Bind(local_id, _meta) => ast::Type {
            node: ast::TypeNode::Var(type_var_id_for_local(*local_id)),
        },
        ast::Pattern::Literal(c, _meta) => ast::Type {
            node: ast::TypeNode::Primitive(c.type_()),
        },
        ast::Pattern::Construct(constructor, args, meta) => {
            let args_type: Vec<_> = args
                .iter()
                .map(|p| infer_pattern_type(p, symbols, inferer, e))
                .collect();
            let constructor = symbols.constructor(&constructor);
            let data = symbols.data(constructor.belongs_to);

            let type_var_zero = inferer.new_slots(data.kind_arity);
            let data_params = (0..data.kind_arity)
                .map(|i| ast::Type::new_var(type_var_zero.offset(i)))
                .collect();

            let params: Vec<_> = constructor
                .params
                .iter()
                .map(|p| p.offset_vars(type_var_zero))
                .collect();

            if let Err(err_i) = inferer.unify_list(args_type.iter(), params.iter()) {
                e.push(errors::PatternUnificationFailure::new(
                    &args_type[err_i],
                    &params[err_i],
                    err_i,
                    args[err_i].meta(),
                    meta,
                ));
                return ast::Type::new_never();
            }
            ast::Type {
                node: ast::TypeNode::Generic(constructor.belongs_to, data_params),
            }
        }
        ast::Pattern::Tuple(v, _meta) => ast::Type {
            node: ast::TypeNode::Tuple(
                v.iter()
                    .map(|x| infer_pattern_type(x, symbols, inferer, e))
                    .collect(),
            ),
        },
    }
}

fn infer_case<'s>(
    operand: ast::ExprId,
    arms: &[ast::CaseArm<'s>],
    case_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let operand = infer_expr(
        operand,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        e,
    );
    let ret_type = ast::Type::new_var(inferer.alloc_var());

    let typed_arms = arms
        .iter()
        .map(|arm| {
            let result = infer_expr(
                arm.result,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                e,
            );

            let guard = arm.guard.map(|g| {
                let guard_expr = infer_expr(
                    g,
                    symbols,
                    ast_nodes,
                    typed_nodes,
                    typed_functions,
                    inferer,
                    e,
                );
                inferer
                    .unify(
                        &guard_expr.type_,
                        &ast::Type {
                            node: ast::TypeNode::Primitive(ast::PrimitiveType::Bool),
                        },
                    )
                    .map_err(|_| {
                        errors::NonBoolTest::new(&guard_expr.type_, &guard_expr.meta, case_meta)
                    })
                    .commit(e);

                typed_nodes.push(guard_expr)
            });

            inferer
                .unify(&ret_type, &result.type_)
                .map_err(|_| {
                    errors::ArmTypeUnificationFailure::new(&result.type_, &result.meta, case_meta)
                })
                .commit(e);

            if let Err(err) = arm.pattern.validate(symbols) {
                e.push_boxed(err);
                return typed::CaseArm {
                    pattern: arm.pattern.clone(),
                    guard,
                    result: typed_nodes.push(result),
                };
            }

            // Infer type of pattern
            let pat_type = infer_pattern_type(&arm.pattern, symbols, inferer, e);

            inferer
                .unify(&operand.type_, &pat_type)
                .map_err(|_| {
                    errors::UnificationFailure::new(
                        &operand.type_,
                        &operand.meta,
                        &pat_type,
                        arm.pattern.meta(),
                        case_meta,
                    )
                })
                .commit(e);
            typed::CaseArm {
                pattern: arm.pattern.clone(),
                guard,
                result: typed_nodes.push(result),
            }
        })
        .collect();

    typed::Expr {
        node: typed::ExprNode::Case {
            operand: typed_nodes.push(operand),
            arms: typed_arms,
        },
        type_: ret_type,
        meta: case_meta.clone(),
    }
}

fn infer_local(id: ast::LocalId, _mutable: bool, id_meta: &ast::Meta) -> typed::Expr<'static> {
    typed::Expr {
        node: typed::ExprNode::Local(id),
        type_: ast::Type::new_var(type_var_id_for_local(id)),
        meta: id_meta.clone(),
    }
}

fn infer_captured<'s>(
    id: ast::CapturedId,
    is_self: bool,
    _mutable: bool,
    id_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
) -> typed::Expr<'static> {
    let local_cnt = symbols
        .function(typed_functions.currently_infering().unwrap())
        .local_cnt;
    typed::Expr {
        node: typed::ExprNode::Captured { id, is_self },
        type_: ast::Type::new_var(type_var_id_for_captured(local_cnt, id)),
        meta: id_meta.clone(),
    }
}

/// Infer type of a function and return its signature renamed
fn infer_function_type_renamed<'s>(
    name: &str,
    name_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> Option<ast::FunctionType<'s>> {
    // Already infered its type
    if let Some(infered_f) = typed_functions.get(name) {
        let renamed_type = inferer.rename_type_vars(&infered_f.type_, infered_f.var_cnt);
        return Some(renamed_type);
    }

    // Type annotated by programmer
    let f = symbols.function(name);
    if let Some(f_type) = &f.type_ {
        return Some(inferer.rename_type_vars(&f_type.clone().into(), f.var_cnt));
    }

    // Not in infering tree
    if !typed_functions.is_infering(name) {
        if let Some(infered_f) =
            infer_function(name, symbols, ast_nodes, typed_nodes, typed_functions, e)
        {
            let renamed_type = inferer.rename_type_vars(&infered_f.type_, infered_f.var_cnt);
            typed_functions.insert(name.to_owned(), infered_f);
            return Some(renamed_type);
        }
    }

    // In infering tree, but is a self-recursion. This is a common case worth special treatment.
    // Arguments we are passing are certainly more specified than parameters, so we use a upper-bound here
    // to represent the relationship. The same is for return value.
    // And we will never infer closure recursively, so no closure-inference will reach here
    if typed_functions.currently_infering().unwrap() == name {
        let mut currently_infered = ast::FunctionType {
            params: (0..f.arity)
                .map(|i| {
                    inferer
                        .resolve_var(type_var_id_for_local(i.into()), &f.param_metas[i])
                        .unwrap_or_else(|err| {
                            e.push(err);
                            ast::Type::new_never()
                        })
                })
                .collect(),
            captures: (0..f.captures.len())
                .map(|i| {
                    inferer
                        .resolve_var(
                            type_var_id_for_captured(f.local_cnt, i.into()),
                            &f.param_metas[i],
                        )
                        .unwrap_or_else(|err| {
                            e.push(err);
                            ast::Type::new_never()
                        })
                })
                .collect(),
            ret: Box::new(
                inferer
                    .resolve_var(type_var_id_for_ret(f.local_cnt), &f.meta)
                    .unwrap_or_else(|err| {
                        e.push(err);
                        ast::Type::new_never()
                    }),
            ),
        };

        let mut vars = HashSet::new();
        currently_infered.collect_vars(&mut vars);
        let new_vars = inferer.alloc_vars(vars.len());
        let var_subs: HashMap<_, _> = vars
            .into_iter()
            .zip(new_vars)
            .map(|(from, to)| (from, ast::Type::new_var(to)))
            .collect();

        currently_infered.substitute_vars(&|id| var_subs[&id].clone());

        return Some(currently_infered);
    }

    e.push(errors::CircularInference::new(name, name_meta));
    None
}

/// Init the [`inferer::Inferer`] for infering function `f`.
///
/// # Special type variables
/// `0..f.arity` are assigned to parameters of `f`
/// `0..f.locals_cnt ` are assigned to locals of f. Actually, `f`'s parameters are exactly the first few locals
/// `f.locals_cnt` is assigned to return value of `f`
/// `(f.locals_cnt + 1)..(f.locals_cnt + 1 + f.captures_cnt)` are assigned to captured values of `f`
///
/// This function allocate locals, unify user provided function signature and returns the type variable representing
/// return value of the function
fn init_inferer_for_function_inference<'s>(
    f: &ast::Function<'s>,
    symbols: &ast::Symbols<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> ast::Type<'s> {
    // allocate type-vars for locals and return-value and captures
    let _ = inferer.alloc_vars(f.local_cnt + 1 + f.captures.len());

    let ret_type = ast::Type::new_var(type_var_id_for_ret(f.local_cnt));

    // User provided function signature
    if let Some(f_type) = &f.type_ {
        // Validate user provided function signature
        if let Err(err) = symbols.validate_callable_type(f_type, &f.meta) {
            e.push_boxed(err);
            return ret_type;
        }
        // Allocate type-vars for those in user provided function signature and rename
        let f_type = inferer.rename_type_vars(f_type, f.var_cnt);
        // Unify parameters
        for i in 0..f_type.params.len() {
            inferer.update_var(i.into(), &f_type.params[i]).unwrap()
        }
        inferer
            .update_var(type_var_id_for_ret(f.local_cnt), &f_type.ret)
            .unwrap()
    };

    ret_type
}

fn type_var_id_for_local(local: ast::LocalId) -> ast::TypeVarId {
    ast::TypeVarId::from(local.0)
}

fn type_var_id_for_ret(local_cnt: usize) -> ast::TypeVarId {
    ast::TypeVarId::from(local_cnt)
}

fn type_var_id_for_captured(local_cnt: usize, captured: ast::CapturedId) -> ast::TypeVarId {
    ast::TypeVarId::from(captured.0 + local_cnt + 1)
}

/// Infer the type a normal function.
pub fn infer_function<'s>(
    name: &str,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    e: &mut errors::ManyError,
) -> Option<typed::Function<'s>> {
    let f = symbols.function(name);

    if f.arity > u8::MAX as usize {
        e.push(errors::TooManyParameters::new(&f.meta, f.arity));
    }

    let mut inferer = inferer::Inferer::new();

    let ret_type = init_inferer_for_function_inference(f, symbols, &mut inferer, e);

    typed_functions.begin_infering(name.to_owned());
    // Resolve type of function body
    let body_expr = infer_expr(
        f.body,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        &mut inferer,
        e,
    );

    inferer
        .unify(&ret_type, &body_expr.type_)
        .map_err(|_| errors::BodyTypeMismatchAnnotation::new(&body_expr.type_, &body_expr.meta))
        .commit(e);

    // Construct type of function
    let f_type = ast::FunctionType {
        params: (0..f.arity)
            .zip(f.param_metas.iter())
            .map(|(i, m)| {
                inferer
                    .resolve_var(type_var_id_for_local(i.into()), m)
                    .unwrap_or_else(|err| {
                        e.push(err);
                        ast::Type::new_never()
                    })
            })
            .collect(),
        captures: (0..f.captures.len())
            .map(|i| {
                inferer
                    .resolve_var(
                        type_var_id_for_captured(f.local_cnt, i.into()),
                        &f.captures_meta[i],
                    )
                    .unwrap_or_else(|err| {
                        e.push(err);
                        ast::Type::new_never()
                    })
            })
            .collect(),
        ret: Box::new(
            inferer
                .resolve(&body_expr.type_, &body_expr.meta)
                .unwrap_or_else(|err| {
                    e.push(err);
                    ast::Type::new_never()
                }),
        ),
    };

    use ast::TypeApply;

    // Discretize type of function such that variables are the first few unsigned integers
    // e.g. from 2, 3 -> 4 to 0, 1 -> 2
    let (map, var_cnt) = inferer.discretization_function(&f_type);
    let f_type = f_type.substitute_vars_with_nodes(|i| ast::TypeNode::Var(map[&i]));

    typed_functions.finish_infering();
    let body = typed_nodes.push(body_expr);

    typed_nodes.walk(body, &mut |expr| {
        let resolved = inferer
            .resolve(&expr.type_, &expr.meta)
            .unwrap_or_else(|err| {
                e.push(err);
                ast::Type::new_never()
            });
            
        expr.type_ = resolved.substitute_vars_with_nodes(|i| {
            map.get(&i).map_or_else(
                || {
                    e.push(errors::UnboundTypeVariable::new(&expr.meta));
                    ast::TypeNode::Never
                },
                |x| ast::TypeNode::Var(*x),
            )
        });
    });

    Some(typed::Function {
        var_cnt,
        body,
        meta: f.meta.clone(),
        type_: f_type,
        captures: f.captures.clone(),
        local_cnt: f.local_cnt,
    })
}
