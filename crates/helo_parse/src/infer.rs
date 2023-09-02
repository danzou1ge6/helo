use crate::ast;
use crate::errors;
use crate::inferer;
use crate::typed;
use errors::ManyErrorReceive;

pub fn infer_expr<'s>(
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
        ExprNode::Call { callee, args } => infer_call(
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
        ExprNode::Closure(closure) => infer_closure(
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
        ExprNode::TupleGet(from, index) => infer_tuple_get(
            *from,
            *index,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ),
        ExprNode::Constant(constant) => infer_constant(constant, &expr.meta),
        ExprNode::Local(id) => infer_local(*id, &expr.meta),
    };
    if let Some(provided_type) = &expr.type_ {
        let provided_type = inferer.instantiate_wildcard(provided_type);
        inferer
            .unify(&provided_type, &typed_expr.type_, &expr.meta)
            .commit(e);
    }
    typed_expr
}

pub fn infer_expr_many<'s, 'a>(
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

fn infer_tuple_get<'s>(
    from: ast::ExprId,
    index: usize,
    get_meta: &ast::Meta,
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

    let type_ = match inferer
        .resolve(&from.type_)
        .unwrap_or_else(|err| {
            e.push(err);
            ast::Type::new_never(get_meta.clone())
        })
        .node
    {
        ast::TypeNode::Tuple(elements_type) => {
            if elements_type.len() <= index {
                ast::Type::new_never(get_meta.clone())
            } else {
                elements_type[index].clone()
            }
        }
        ast::TypeNode::Var(_) => {
            e.push(errors::TypeAnnotationRequiredForTuple::new(&from.meta));
            ast::Type::new_never(get_meta.clone())
        }
        otherwise => {
            if !matches!(otherwise, ast::TypeNode::Never) {
                e.push(errors::NoTupleAccess::new(&from.type_));
            }
            ast::Type::new_never(get_meta.clone())
        }
    };

    typed::Expr {
        node: typed::ExprNode::TupleGet(typed_nodes.push(from), index),
        type_,
        meta: get_meta.clone(),
    }
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
            meta: tuple_meta.clone(),
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

    let ret_type = match inferer
        .resolve(&callee.type_)
        .unwrap_or_else(|err| {
            e.push(err);
            ast::Type::new_never(call_meta.clone())
        })
        .node
    {
        // NOTE that we assume that type-vars have already been renamed
        ast::TypeNode::Callable(ast::CallableType { params, ret }) => {
            inferer
                .unify_list(args.iter().map(|a| &a.type_), params.iter(), call_meta)
                .commit(e);
            ret.as_ref().clone()
        }
        ast::TypeNode::Var(v_id) => {
            let ret_type = ast::Type {
                node: ast::TypeNode::Var(inferer.alloc_var()),
                meta: call_meta.clone(),
            };
            inferer
                .update_var(
                    v_id,
                    &ast::Type {
                        node: ast::TypeNode::Callable(ast::CallableType {
                            params: args.iter().map(|a| a.type_.clone()).collect(),
                            ret: Box::new(ret_type.clone()),
                        }),
                        meta: call_meta.clone(),
                    },
                    call_meta,
                )
                .commit(e);
            ret_type
        }
        otherwise => {
            if !matches!(otherwise, ast::TypeNode::Never) {
                e.push(errors::NotCallable::new(&callee.type_));
            }
            ast::Type::new_never(call_meta.clone())
        }
    };

    typed::Expr {
        node: typed::ExprNode::Call {
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
        .update_var(type_var_id_for_local(bind), &value.type_, &value.meta)
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
    dbg!(&ret_type);
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
        .unify(&pattern_type, &value.type_, let_in_meta)
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
    dbg!(&ret_type);
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
            f,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        ) {
            return typed::Expr {
                node: typed::ExprNode::Global(name),
                type_: ast::Type {
                    node: ast::TypeNode::Callable(renamed_f_type),
                    meta: f.meta.clone(),
                },
                meta: global_meta.clone(),
            };
        }
        // Fail to infer `name`'s signature, but that's not a error here
        // So we fail with never type silently

        // Global is builtin
    } else if let Some(f) = symbols.get_builtin_type(name) {
        let renamed_type = inferer.rename_callable_type_vars(&f.type_, f.var_cnt);
        let r = typed::Expr {
            node: typed::ExprNode::Global(name),
            type_: ast::Type {
                node: ast::TypeNode::Callable(renamed_type),
                meta: f.meta.clone(),
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
                    .map(|i| ast::Type::new_var(i.into(), data.generic_metas[i].clone()))
                    .collect(),
            ),
            meta: data.meta.clone(),
        };
        let type_ = ast::CallableType {
            params: constructor.params.clone(),
            ret: Box::new(ret_type),
        };
        let type_ = inferer.rename_callable_type_vars(&type_, data.kind_arity);
        let type_ = if type_.params.len() == 0 {
            *type_.ret
        } else {
            ast::Type {
                node: ast::TypeNode::Callable(type_),
                meta: constructor.meta.clone(),
            }
        };
        dbg!(&type_);
        return typed::Expr {
            node: typed::ExprNode::Global(name),
            type_,
            meta: global_meta.clone(),
        };

    // Fail: global doesn't exist
    } else {
        e.push(errors::GlobalNotFound::new(name, global_meta));
    }

    // Fail: fallthrough
    typed::Expr {
        node: typed::ExprNode::Global(name),
        type_: ast::Type::new_never(global_meta.clone()),
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
    let ret_type = inferer
        .unify(&then.type_, &else_.type_, if_else_meta)
        .map_or_else(
            |err| {
                e.push(err);
                ast::Type::new_never(if_else_meta.clone())
            },
            |_| then.type_.clone(),
        );

    // Test clause must be bool
    inferer
        .unify(
            &test.type_,
            &ast::Type {
                node: ast::TypeNode::Primitive(ast::PrimitiveType::Bool),
                meta: test.meta.clone(),
            },
            &test.meta,
        )
        .map_or_else(|err| e.push(err), |x| x);

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
            meta: constant_meta.clone(),
        },
        meta: constant_meta.clone(),
    }
}

fn infer_closure<'s>(
    closure: &ast::Closure<'s>,
    closure_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    // Infer closure function
    let closure_id = closure_meta.closure_id();

    // TODO: the last parameter passed to the closure function is a tuple, this must be known during inference of closure function

    let closure_type = if let Some(infered_f) = infer_closure_function(
        &closure_id,
        &closure.function,
        &closure.captures_meta,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        e,
    ) {
        let mut renamed_type =
            inferer.rename_callable_type_vars(&infered_f.type_, infered_f.var_cnt);

        let captured_tuple_type = ast::Type {
            node: ast::TypeNode::Tuple(
                closure
                    .captures
                    .iter()
                    .zip(closure.captures_meta.iter())
                    .map(|(local, meta)| {
                        ast::Type::new_var(type_var_id_for_local(*local), meta.clone())
                    })
                    .collect(),
            ),
            meta: closure_meta.clone(),
        };

        // Unify captured locals with the last parameter of the closure function,
        // so as to get the type of the closure
        let arity = renamed_type.params.len() - 1;
        inferer
            .unify(
                &renamed_type.params[arity],
                &captured_tuple_type,
                closure_meta,
            )
            .commit(e);

        // Throw away the last few arguments corresponding to captured locals
        renamed_type.params.resize_with(arity, || unreachable!());
        let closure_type = ast::Type {
            node: ast::TypeNode::Callable(renamed_type),
            meta: closure_meta.clone(),
        };
        typed_functions.insert(closure_id.clone(), infered_f);
        closure_type

    // Fail: fail to infer closure function, use dummy never type, but throw no error
    } else {
        ast::Type::new_never(closure_meta.clone())
    };
    return typed::Expr {
        node: typed::ExprNode::Closure(typed::Closure {
            function: closure_id,
            captures: closure.captures.iter().map(|i| *i).collect(),
            captures_meta: closure.captures_meta.clone(),
        }),
        type_: closure_type,
        meta: closure_meta.clone(),
    };
}

pub fn infer_pattern_type<'s>(
    pattern: &ast::Pattern<'s>,
    symbols: &ast::Symbols<'s>,
    inferer: &mut crate::inferer::Inferer<'s>,
    e: &mut crate::errors::ManyError,
) -> ast::Type<'s> {
    match pattern {
        ast::Pattern::Bind(local_id, meta) => ast::Type {
            node: ast::TypeNode::Var(type_var_id_for_local(*local_id)),
            meta: meta.clone(),
        },
        ast::Pattern::Literal(c, meta) => ast::Type {
            node: ast::TypeNode::Primitive(c.type_()),
            meta: meta.clone(),
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
                .map(|i| ast::Type::new_var(type_var_zero.offset(i), data.generic_metas[i].clone()))
                .collect();

            let params: Vec<_> = constructor
                .params
                .iter()
                .map(|p| p.offset_vars(type_var_zero))
                .collect();

            if let Err(err) = inferer.unify_list(args_type.iter(), params.iter(), meta) {
                e.push(err);
                return ast::Type::new_never(meta.clone());
            }
            ast::Type {
                node: ast::TypeNode::Generic(constructor.belongs_to, data_params),
                meta: meta.clone(),
            }
        }
        ast::Pattern::Tuple(v, meta) => ast::Type {
            node: ast::TypeNode::Tuple(
                v.iter()
                    .map(|x| infer_pattern_type(x, symbols, inferer, e))
                    .collect(),
            ),
            meta: meta.clone(),
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
    let ret_type = ast::Type::new_var(inferer.alloc_var(), case_meta.clone());

    for arm in arms {
        if let Err(err) = arm.pattern.validate(symbols) {
            e.push_boxed(err);
            continue;
        }
        // Infer type of pattern
        let pat_type = infer_pattern_type(&arm.pattern, symbols, inferer, e);
        let result = infer_expr(
            arm.result,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            e,
        );

        inferer
            .unify(&operand.type_, &pat_type, arm.pattern.meta())
            .commit(e);
        inferer
            .unify(&ret_type, &result.type_, &result.meta)
            .commit(e);
    }
    typed::Expr {
        node: typed::ExprNode::Case {
            operand: typed_nodes.push(operand),
            arms: arms.to_vec(),
        },
        type_: ret_type,
        meta: case_meta.clone(),
    }
}

fn infer_local(id: ast::LocalId, id_meta: &ast::Meta) -> typed::Expr<'static> {
    typed::Expr {
        node: typed::ExprNode::Local(id),
        type_: ast::Type::new_var(type_var_id_for_local(id), id_meta.clone()),
        meta: id_meta.clone(),
    }
}

/// Infer type of a function and return its signature renamed
pub fn infer_function_type_renamed<'s>(
    name: &str,
    name_meta: &ast::Meta,
    f: &ast::Function<'s>,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> Option<ast::CallableType<'s>> {
    // Already infered its type
    if let Some(infered_f) = typed_functions.get(name) {
        let renamed_type = inferer.rename_callable_type_vars(&infered_f.type_, infered_f.var_cnt);
        return Some(renamed_type);
    }

    // Type annotated by programmer
    if let Some(f_type) = &f.type_ {
        return Some(inferer.rename_callable_type_vars(f_type, f.var_cnt));
    }

    // Not in infering tree
    if !typed_functions.is_infering(name) {
        if let Some(infered_f) =
            infer_function(name, f, symbols, ast_nodes, typed_nodes, typed_functions, e)
        {
            let renamed_type =
                inferer.rename_callable_type_vars(&infered_f.type_, infered_f.var_cnt);
            typed_functions.insert(name.to_owned(), infered_f);
            return Some(renamed_type);
        }
    }

    // In infering tree, but is a self-recursion. This is a common case worth special treatment.
    if typed_functions.currently_infering().unwrap() == name {
        let r = Some(ast::CallableType {
            params: (0..f.arity)
                .map(|i| {
                    ast::Type::new_bounded_var(
                        type_var_id_for_local(i.into()),
                        f.param_metas[i].clone(),
                    )
                })
                .collect(),
            ret: Box::new(ast::Type::new_bounded_var(
                type_var_id_for_local(f.local_cnt.into()),
                f.meta.clone(),
            )),
        });
        dbg!(&r);
        return r;
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
///
/// This function allocate locals, unify user provided function signature and returns the type variable representing
/// return value of the function
fn init_inferer_for_function_inference<'s>(
    f: &ast::Function<'s>,
    symbols: &ast::Symbols<'s>,
    inferer: &mut inferer::Inferer<'s>,
    e: &mut errors::ManyError,
) -> ast::Type<'s> {
    let _ = inferer.alloc_vars(f.local_cnt + 1); // allocate type-vars for locals and return-value

    let ret_type = ast::Type::new_var(f.local_cnt.into(), f.meta.clone());

    // User provided function signature
    if let Some(f_type) = &f.type_ {
        // Validate user provided function signature
        if let Err(err) = symbols.validate_callable_type(f_type) {
            e.push_boxed(err);
            return ret_type;
        }
        // Allocate type-vars for those in user provided function signature and rename
        let f_type = inferer.rename_callable_type_vars(f_type, f.var_cnt);
        // Unify parameters
        for i in 0..f_type.params.len() {
            inferer
                .update_var(i.into(), &f_type.params[i], &f.param_metas[i])
                .commit(e);
        }
        inferer
            .update_var(f.local_cnt.into(), &f_type.ret, &f.meta)
            .commit(e);
    };

    ret_type
}

fn type_var_id_for_local(local: ast::LocalId) -> ast::TypeVarId {
    ast::TypeVarId::from(local.0)
}

/// Infer type of a closure function, that is, the last parameter of the function is the tuple of captured locals
pub fn infer_closure_function<'s>(
    name: &str,
    f: &ast::Function<'s>,
    captures_meta: &[ast::Meta],
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    e: &mut errors::ManyError,
) -> Option<typed::Function<'s>> {
    let mut inferer = inferer::Inferer::new();
    let ret_type = init_inferer_for_function_inference(f, symbols, &mut inferer, e);

    // Let the inferer know that the last argument is a tuple
    let captures_cnt = captures_meta.len();
    let captures_tuple_type = ast::Type {
        node: ast::TypeNode::Tuple(
            inferer
                .alloc_type_vars(captures_cnt, &|i| captures_meta[i].clone())
                .collect(),
        ),
        meta: f.meta.clone(),
    };
    inferer
        .update_var((f.arity - 1).into(), &captures_tuple_type, &f.meta)
        .commit(e);

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
        .unify(&ret_type, &body_expr.type_, &f.meta)
        .commit(e);

    // Construct type of function
    let f_type = ast::CallableType {
        params: (0..f.arity)
            .map(|i| {
                inferer
                    .resolve_var(
                        type_var_id_for_local(i.into()),
                        if i == f.arity - 1 {
                            &f.meta
                        } else {
                            &f.param_metas[i]
                        },
                    )
                    .unwrap_or_else(|err| {
                        e.push(err);
                        ast::Type::new_never(if i == f.arity - 1 {
                            f.meta.clone()
                        } else {
                            f.param_metas[i].clone()
                        })
                    })
            })
            .collect(),
        ret: Box::new(inferer.resolve(&body_expr.type_).unwrap_or_else(|err| {
            e.push(err);
            ast::Type::new_never(body_expr.meta.clone())
        })),
    };

    // Discretize type of function such that variables are the first few unsigned integers
    // e.g. from 2, 3 -> 4 to 0, 1 -> 2
    let (map, var_cnt) = inferer.discretization_callable(&f_type);
    let f_type = f_type.substitute_vars_with_nodes(&|i| ast::TypeNode::Var(map[&i]));

    typed_functions.finish_infering();

    Some(typed::Function {
        var_cnt,
        body: typed_nodes.push(body_expr),
        meta: f.meta.clone(),
        type_: f_type,
    })
}

/// Infer the type a normal function.
pub fn infer_function<'s>(
    name: &str,
    f: &ast::Function<'s>,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    e: &mut errors::ManyError,
) -> Option<typed::Function<'s>> {
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
        .unify(&ret_type, &body_expr.type_, &f.meta)
        .commit(e);

    // Construct type of function
    let f_type = ast::CallableType {
        params: (0..f.arity)
            .zip(f.param_metas.iter())
            .map(|(i, m)| {
                inferer.resolve_var(i.into(), m).unwrap_or_else(|err| {
                    e.push(err);
                    ast::Type::new_never(f.param_metas[i].clone())
                })
            })
            .collect(),
        ret: Box::new(inferer.resolve(&body_expr.type_).unwrap_or_else(|err| {
            e.push(err);
            ast::Type::new_never(body_expr.meta.clone())
        })),
    };

    // Discretize type of function such that variables are the first few unsigned integers
    // e.g. from 2, 3 -> 4 to 0, 1 -> 2
    let (map, var_cnt) = inferer.discretization_callable(&f_type);
    let f_type = f_type.substitute_vars_with_nodes(&|i| ast::TypeNode::Var(map[&i]));

    typed_functions.finish_infering();

    Some(typed::Function {
        var_cnt,
        body: typed_nodes.push(body_expr),
        meta: f.meta.clone(),
        type_: f_type,
    })
}
