use crate::ast;
use crate::ast::TypeApply;
use crate::constrain;
use crate::errors;
use crate::inferer;
use crate::typed;
use errors::ManyErrorReceive;

#[cfg(debug_assertions)]
const DEBUG: bool = false;

#[cfg(not(debug_assertions))]
const DBUEG: bool = false;

struct TypeMapping {
    generic_var_cnt: usize,
    local_cnt: usize,
}

impl TypeMapping {
    fn local(&self, local: ast::LocalId) -> ast::TypeVarId {
        ast::TypeVarId::from(self.generic_var_cnt + local.0)
    }
    fn ret(&self) -> ast::TypeVarId {
        ast::TypeVarId::from(self.generic_var_cnt + self.local_cnt)
    }
    fn captured(&self, captured: ast::CapturedId) -> ast::TypeVarId {
        ast::TypeVarId::from(captured.0 + self.local_cnt + 1 + self.generic_var_cnt)
    }
    fn capture(&self, capture: ast::Capture) -> ast::TypeVarId {
        match capture {
            ast::Capture::Local(id, _) => self.local(id),
            ast::Capture::Capture(id, _) => self.captured(id),
        }
    }
}

fn infer_expr<'s>(
    expr_id: ast::ExprId,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
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
            assumptions,
            type_mapping,
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
            assumptions,
            type_mapping,
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
            assumptions,
            type_mapping,
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
            assumptions,
            type_mapping,
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
            inferer,
            type_mapping,
            e,
        ),
        ExprNode::MakeClosureAt {
            at,
            f,
            captures,
            then,
        } => infer_make_closure_at(
            *at,
            f,
            captures,
            *then,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            assumptions,
            type_mapping,
            e,
        ),
        ExprNode::MakeClosure { f, captures } => infer_make_closure(
            f,
            captures,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            type_mapping,
            e,
        ),
        ExprNode::UserFunction(name) => infer_user_function(
            name,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            assumptions,
            type_mapping,
            e,
        ),
        ExprNode::Constructor(name) => infer_constructor(
            name,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            assumptions,
            type_mapping,
            e,
        ),
        ExprNode::Builtin(name) => infer_builtin(
            name,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            assumptions,
            type_mapping,
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
            assumptions,
            type_mapping,
            e,
        ),
        ExprNode::Constant(constant) => infer_constant(constant, &expr.meta),
        ExprNode::Local(id, mutable) => infer_local(*id, *mutable, &expr.meta, type_mapping),
        ExprNode::Seq(stmts, result) => infer_seq(
            stmts.iter(),
            *result,
            &expr.meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            assumptions,
            type_mapping,
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
            assumptions,
            type_mapping,
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
        ExprNode::Never => panic!("Never from untyped AST should not propagate to typed AST"),
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
                    inferer,
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
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
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
                assumptions,
                type_mapping,
                e,
            );
            let then = infer_expr(
                *then,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                assumptions,
                type_mapping,
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
                assumptions,
                type_mapping,
                e,
            );
            let then = infer_expr(
                *then,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                assumptions,
                type_mapping,
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
                assumptions,
                type_mapping,
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
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
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
                assumptions,
                type_mapping,
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
            assumptions,
            type_mapping,
            e,
        )
    });

    let type_ = result
        .as_ref()
        .map_or(ast::Type::new(ast::TypeNode::Unit), |expr| {
            expr.type_.clone()
        });

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
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let from = infer_expr(
        from,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
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
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
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
                assumptions,
                type_mapping,
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
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let elements = infer_expr_many(
        elements.into_iter(),
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
        e,
    );
    let type_ = elements.iter().map(|e| e.type_.clone()).collect();
    let elements = typed_nodes.push_many(elements.into_iter());
    typed::Expr {
        node: typed::ExprNode::Tuple(elements),
        type_: ast::Type::new(ast::TypeNode::Tuple(type_)),
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
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let callee = infer_expr(
        callee,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
        e,
    );
    let args = infer_expr_many(
        args.iter(),
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
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
                        &inferer,
                    )
                })
                .commit(e);

            if callee_type_node.impure()
                && symbols
                    .functions
                    .get(typed_functions.currently_infering().unwrap())
                    .unwrap()
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
                let ret_type = ast::Type::new(type_constructor(ret_callable_type));
                ret_type
            }
        }
        ast::TypeNode::Var(v_id) => {
            let ret_type = ast::Type::new(ast::TypeNode::Var(inferer.alloc_free_var()));

            let type_constructor = if symbols
                .functions
                .get(typed_functions.currently_infering().unwrap())
                .unwrap()
                .pure
            {
                ast::TypeNode::Callable
            } else {
                ast::TypeNode::ImpureCallable
            };

            inferer
                .update_var(
                    *v_id,
                    &ast::Type::new(type_constructor(ast::CallableType {
                        params: args.iter().map(|a| a.type_.clone()).collect(),
                        ret: Box::new(ret_type.clone()),
                    })),
                )
                .map_err(|_| {
                    errors::ArgumentsUnificationFailure::new(
                        &callee.type_,
                        args.iter().map(|arg| &arg.type_),
                        call_meta,
                        &inferer,
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

fn _infer_let_in<'s>(
    bind: ast::LocalId,
    value: ast::ExprId,
    in_: ast::ExprId,
    let_in_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let value = infer_expr(
        value,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
        e,
    );
    // Bind type of value to local
    inferer
        .update_var(type_mapping.local(bind), &value.type_)
        .map_err(|_| {
            errors::LocalUnificationFailure::new(
                bind,
                &value.type_,
                &value.meta,
                let_in_meta,
                &inferer,
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
        assumptions,
        type_mapping,
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
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    if !pattern.inrefutable(symbols) {
        e.push(errors::RefutablePattern::new(pattern.meta()));
    }

    let pattern_type = infer_pattern_type(pattern, symbols, inferer, type_mapping, e);
    let value = infer_expr(
        value,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
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
                inferer,
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
        assumptions,
        type_mapping,
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

fn infer_user_function<'s>(
    name: &ast::FunctionName<'s>,
    meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    _assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let id = ast::FunctionId::of_standard(name.clone());
    if let Some(f) = symbols.functions.get(&id) {
        if let Some(renamed_f_type) = infer_function_type_renamed(
            id.clone(),
            &CapturedTypeInfo::empty(),
            meta,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            inferer,
            type_mapping,
            e,
        ) {
            let type_constructor = if f.pure {
                ast::TypeNode::Callable
            } else {
                ast::TypeNode::ImpureCallable
            };

            return typed::Expr {
                node: typed::ExprNode::UserFunction(id),
                type_: ast::Type {
                    node: type_constructor(renamed_f_type.into()),
                },
                meta: meta.clone(),
            };
        }
    } else if let Some(rel_name) = symbols.methods.get(name) {
        let sig = symbols
            .relations
            .get(rel_name)
            .unwrap()
            .method_sig(name.id());
        let renamed_sig = inferer.rename_type_vars_free(sig, sig.var_cnt);

        let type_constructor = if sig.pure {
            ast::TypeNode::Callable
        } else {
            ast::TypeNode::ImpureCallable
        };

        let r = typed::Expr {
            node: typed::ExprNode::UnresolvedMethod {
                rel_name: rel_name.clone(),
                f_name: name.clone(),
                constrains: renamed_sig.constrains.clone(),
                primary_constrain: renamed_sig.primary_constrain.clone(),
            },
            type_: ast::Type {
                node: type_constructor(renamed_sig.type_),
            },
            meta: meta.clone(),
        };
        return r;
    }
    e.push(errors::FunctionNotFound::new(name, meta));
    typed::Expr::new_never(meta)
}

fn infer_builtin<'s>(
    name: &ast::BuiltinFunctionName<'s>,
    meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    _ast_nodes: &ast::ExprHeap<'s>,
    _typed_nodes: &mut typed::ExprHeap<'s>,
    _typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    _assumptions: &constrain::Assumptions<'s>,
    _type_mapping: &TypeMapping,
    _e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let f = symbols.builtins.get(name).unwrap();
    let renamed_type = inferer.rename_type_vars_free(&f.type_, f.var_cnt);

    let type_constructor = if f.pure {
        ast::TypeNode::Callable
    } else {
        ast::TypeNode::ImpureCallable
    };

    let r = typed::Expr {
        node: typed::ExprNode::Builtin(name.clone()),
        type_: ast::Type {
            node: type_constructor(renamed_type),
        },
        meta: meta.clone(),
    };
    r
}

fn infer_constructor<'s>(
    name: &ast::ConstructorName<'s>,
    meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    _ast_nodes: &ast::ExprHeap<'s>,
    _typed_nodes: &mut typed::ExprHeap<'s>,
    _typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    _assumptions: &constrain::Assumptions<'s>,
    _type_mapping: &TypeMapping,
    _e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let constructor = symbols.constructors.get(name).unwrap();
    let data = &symbols.datas.get(&constructor.belongs_to).unwrap();
    let ret_type = ast::Type {
        node: ast::TypeNode::Generic(
            constructor.belongs_to.clone(),
            (0..data.kind_arity)
                .map(|i| ast::Type::new_var(i.into()))
                .collect(),
        ),
    };
    let type_ = ast::CallableType {
        params: constructor.params.clone(),
        ret: Box::new(ret_type),
    };
    let type_ = inferer.rename_type_vars_free(&type_, data.kind_arity);

    let type_ = if type_.params.len() == 0 {
        *type_.ret
    } else {
        ast::Type {
            node: ast::TypeNode::Callable(type_),
        }
    };
    return typed::Expr {
        node: typed::ExprNode::Constructor(name.clone()),
        type_,
        meta: meta.clone(),
    };
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
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let test = infer_expr(
        test,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
        e,
    );
    let then = infer_expr(
        then,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
        e,
    );
    let else_ = infer_expr(
        else_,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
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
                inferer,
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

pub struct CapturedTypeInfo<'s> {
    types: Vec<ast::Type<'s>>,
    var_cnt: usize,
    metas: Vec<ast::Meta>,
}

impl<'s> CapturedTypeInfo<'s> {
    pub fn empty() -> Self {
        Self {
            types: Vec::new(),
            var_cnt: 0,
            metas: Vec::new(),
        }
    }
}

fn infer_closure<'s>(
    at: Option<ast::LocalId>,
    f_id: &ast::FunctionId<'s>,
    captures: &Vec<ast::Capture>,
    meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> Option<ast::FunctionType<'s>> {
    // Check purity
    if !symbols.functions.get(&f_id).unwrap().pure
        && symbols
            .functions
            .get(typed_functions.currently_infering().unwrap())
            .unwrap()
            .pure
    {
        e.push(errors::InpureClosureInPureFunction::new(&meta));
    }

    let f = symbols.functions.get(f_id).unwrap();

    // Assign local bound to the closure a type, if any
    if let Some(f_type) = &f.type_ {
        if let Some(at) = at {
            let callable_constructor = if f.pure {
                ast::TypeNode::Callable
            } else {
                ast::TypeNode::ImpureCallable
            };
            let renamed_ftype = inferer.rename_type_vars_free(f_type, f.var_cnt);
            inferer
                .update_var(
                    type_mapping.local(at),
                    &ast::Type::new(callable_constructor(renamed_ftype)),
                )
                .expect("`at` should be a fresh type var, and updating it shouldn't go wrong");
        }
    }

    // Collect type of captures
    let captured_types = captures
        .iter()
        .map(|c| {
            inferer
                .resolve_var(type_mapping.capture(c.clone()), c.meta())
                .unwrap_or_else(|err| {
                    e.push(err);
                    ast::Type::new_never()
                })
        })
        .collect::<Vec<_>>();
    let (captured_types_dis, captured_types_var_cnt) =
        inferer.discretization_applied(&captured_types);

    let captured_type_info = CapturedTypeInfo {
        types: captured_types_dis,
        var_cnt: captured_types_var_cnt,
        metas: captures.iter().map(|c| c.meta().clone()).collect(),
    };

    match infer_function_type_renamed(
        f_id.clone(),
        &captured_type_info,
        meta,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        type_mapping,
        e,
    ) {
        Some(f_type) => {
            // Unify types for each capture
            f_type
                .captures
                .iter()
                .zip(captures.iter())
                .for_each(|(ctype, cap)| {
                    inferer
                        .update_var(type_mapping.capture(cap.clone()), ctype)
                        .map_err(|_| {
                            errors::CaptureUnificationFailure::new(&ctype, cap.meta(), &inferer)
                        })
                        .commit(e);
                });

            Some(f_type)
        }
        None => None,
    }
}

fn infer_make_closure<'s>(
    f_id: &ast::FunctionId<'s>,
    captures: &Vec<ast::Capture>,
    meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    match infer_closure(
        None,
        f_id,
        captures,
        meta,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        type_mapping,
        e,
    ) {
        Some(f_type) => typed::Expr {
            node: typed::ExprNode::MakeClosure {
                f: f_id.clone(),
                type_: f_type.clone(),
                captures: captures.clone(),
            },
            type_: if symbols.functions.get(f_id).unwrap().pure {
                ast::Type::new(ast::TypeNode::Callable(f_type.into()))
            } else {
                ast::Type::new(ast::TypeNode::ImpureCallable(f_type.into()))
            },
            meta: meta.clone(),
        },
        None => typed::Expr {
            node: typed::ExprNode::Never,
            type_: ast::Type::new_never(),
            meta: meta.clone(),
        },
    }
}

fn infer_make_closure_at<'s>(
    at: ast::LocalId,
    f_id: &ast::FunctionId<'s>,
    captures: &Vec<ast::Capture>,
    then: ast::ExprId,
    meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    match infer_closure(
        Some(at),
        f_id,
        captures,
        meta,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        type_mapping,
        e,
    ) {
        Some(f_type) => {
            let f = symbols.functions.get(f_id).unwrap();
            let closure_type = if f.pure {
                ast::Type::new(ast::TypeNode::Callable(f_type.clone().into()))
            } else {
                ast::Type::new(ast::TypeNode::ImpureCallable(f_type.clone().into()))
            };

            inferer
                .update_var(type_mapping.local(at), &closure_type)
                .unwrap_or_else(|_| {
                    e.push(errors::BodyTypeMismatchAnnotation::new(
                        &closure_type,
                        &f.meta,
                        &inferer,
                    ))
                });

            let then = infer_expr(
                then,
                symbols,
                ast_nodes,
                typed_nodes,
                typed_functions,
                inferer,
                assumptions,
                type_mapping,
                e,
            );
            let then_type = then.type_.clone();
            let then_id = typed_nodes.push(then);

            typed::Expr {
                node: typed::ExprNode::MakeClosureAt {
                    f: f_id.clone(),
                    type_: f_type,
                    captures: captures.clone(),
                    at,
                    then: then_id,
                },
                type_: then_type,
                meta: meta.clone(),
            }
        }
        None => typed::Expr {
            node: typed::ExprNode::Never,
            type_: ast::Type::new_never(),
            meta: meta.clone(),
        },
    }
}

fn infer_pattern_type<'s>(
    pattern: &ast::Pattern<'s>,
    symbols: &ast::Symbols<'s>,
    inferer: &mut crate::inferer::Inferer<'s>,
    type_mapping: &TypeMapping,
    e: &mut crate::errors::ManyError,
) -> ast::Type<'s> {
    match pattern {
        ast::Pattern::Bind(local_id, _meta) => ast::Type {
            node: ast::TypeNode::Var(type_mapping.local(*local_id)),
        },
        ast::Pattern::Literal(c, _meta) => ast::Type {
            node: ast::TypeNode::Primitive(c.type_()),
        },
        ast::Pattern::Construct(constructor, args, meta) => {
            let args_type: Vec<_> = args
                .iter()
                .map(|p| infer_pattern_type(p, symbols, inferer, type_mapping, e))
                .collect();
            let constructor = symbols.constructors.get(constructor).unwrap();
            let data = symbols.datas.get(&constructor.belongs_to).unwrap();

            let type_var_zero = inferer.new_slots_free(data.kind_arity);
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
                    inferer,
                ));
                return ast::Type::new_never();
            }
            ast::Type {
                node: ast::TypeNode::Generic(constructor.belongs_to.clone(), data_params),
            }
        }
        ast::Pattern::Tuple(v, _meta) => ast::Type {
            node: ast::TypeNode::Tuple(
                v.iter()
                    .map(|x| infer_pattern_type(x, symbols, inferer, type_mapping, e))
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
    assumptions: &constrain::Assumptions<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let operand = infer_expr(
        operand,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        inferer,
        assumptions,
        type_mapping,
        e,
    );
    let ret_type = ast::Type::new_var(inferer.alloc_free_var());

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
                assumptions,
                type_mapping,
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
                    assumptions,
                    type_mapping,
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
                    errors::ArmTypeUnificationFailure::new(
                        &result.type_,
                        &result.meta,
                        case_meta,
                        inferer,
                    )
                })
                .commit(e);

            // Infer type of pattern
            let pat_type = infer_pattern_type(&arm.pattern, symbols, inferer, type_mapping, e);

            inferer
                .unify(&operand.type_, &pat_type)
                .map_err(|_| {
                    errors::UnificationFailure::new(
                        &operand.type_,
                        &operand.meta,
                        &pat_type,
                        arm.pattern.meta(),
                        case_meta,
                        &inferer,
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

fn infer_local(
    id: ast::LocalId,
    _mutable: bool,
    id_meta: &ast::Meta,
    type_mapping: &TypeMapping,
) -> typed::Expr<'static> {
    typed::Expr {
        node: typed::ExprNode::Local(id),
        type_: ast::Type::new_var(type_mapping.local(id)),
        meta: id_meta.clone(),
    }
}

fn infer_captured<'s>(
    id: ast::CapturedId,
    is_self: bool,
    _mutable: bool,
    id_meta: &ast::Meta,
    _symbols: &ast::Symbols<'s>,
    _typed_functions: &mut typed::FunctionTable<'s>,
    _inferer: &mut inferer::Inferer<'s>,
    type_mapping: &TypeMapping,
    _e: &mut errors::ManyError,
) -> typed::Expr<'s> {
    let type_ = { ast::Type::new_var(type_mapping.captured(id)) };

    typed::Expr {
        node: typed::ExprNode::Captured { id, is_self },
        type_,
        meta: id_meta.clone(),
    }
}

fn current_inferred_renamed<'s>(
    f: &ast::Function<'s>,
    inferer: &mut inferer::Inferer<'s>,
    captured_types: &CapturedTypeInfo<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> (ast::FunctionType<'s>, bool) {
    let currently_infered = construct_function_type(f, inferer, captured_types, type_mapping, e);
    // Discretize type of function such that variables are the first few unsigned integers
    // e.g. from 2, 3 -> 4 to 0, 1 -> 2
    let (map, _var_cnt) = inferer.discretization(&currently_infered);
    (
        currently_infered.substitute_vars_with_nodes(|i| ast::TypeNode::Var(map[&i])),
        f.pure,
    )
}

/// Infer type of a function and return its signature renamed
fn infer_function_type_renamed<'s>(
    id: ast::FunctionId<'s>,
    captured_types: &CapturedTypeInfo<'s>,
    name_meta: &ast::Meta,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    inferer: &mut inferer::Inferer<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> Option<ast::FunctionType<'s>> {
    // Already infered its type
    if let Some(infered_f) = typed_functions.get(&id) {
        let renamed_type = inferer.rename_type_vars_free(&infered_f.type_, infered_f.var_cnt);
        return Some(renamed_type);
    }

    // Type annotated by programmer, but is not a closure.
    // This is because a closure must be typed at a `MakeClosure` node, so that we get types for captures
    let f = &symbols.functions[&id];
    if let Some(f_type) = &f.type_ {
        if !id.is_closure() {
            return Some(inferer.rename_type_vars_free(&f_type.clone().into(), f.var_cnt));
        }
    }

    // Not in infering tree
    if !typed_functions.is_infering(&id) {
        if let Some(infered_f) = infer_function(
            id.clone(),
            captured_types,
            symbols,
            ast_nodes,
            typed_nodes,
            typed_functions,
            e,
        ) {
            let renamed_type = inferer.rename_type_vars_free(&infered_f.type_, infered_f.var_cnt);
            typed_functions.insert(id, infered_f);
            return Some(renamed_type);
        }
    }

    // In infering tree, but is a self-recursion. This is a common case worth special treatment.
    if *typed_functions.currently_infering().unwrap() == id {
        let (currently_infered, _) =
            current_inferred_renamed(f, inferer, &captured_types, type_mapping, e);

        return Some(currently_infered);
    }

    e.push(errors::CircularInference::new(id, name_meta));
    None
}

/// Init the [`inferer::Inferer`] for infering function `f`.
///
/// # Special type variables
/// `0..f.var_cnt` are assigned to generic type variables
/// Next `f.locals_cnt ` are assigned to locals of f. Actually, `f`'s parameters are exactly the first few locals
/// `f.var_cnt + f.locals_cnt` is assigned to return value of `f`
/// Next `f.captures_cnt are assigned to captured values of `f`
///
/// This function allocate locals, unify user provided function signature and returns the type variable representing
/// return value of the function
fn init_inferer_for_function_inference<'s>(
    f: &ast::Function<'s>,
    inferer: &mut inferer::Inferer<'s>,
    captured_types: &CapturedTypeInfo<'s>,
    assumptions: Vec<ast::Constrain<'s>>,
) -> (ast::Type<'s>, Vec<ast::Constrain<'s>>, TypeMapping) {
    // If this is a closure, number of generic type variables is stored in
    // `captured_types`;
    // Otherwise `f` knows it
    let var_cnt = if f.capture_cnt == 0 {
        f.var_cnt
    } else {
        captured_types.var_cnt
    };

    // Allocate type-vars for those in user provided function signature and rename.
    // Those type-vars cannot be modified to have concrete type
    let _ = inferer.alloc_locked_vars(var_cnt);
    // allocate type-vars for locals and return-value and captures
    let _ = inferer.alloc_free_vars(f.local_cnt + 1 + f.capture_cnt);

    let type_mapping = TypeMapping {
        local_cnt: f.local_cnt,
        generic_var_cnt: f.var_cnt,
    };

    let ret_type = ast::Type::new_var(type_mapping.ret());

    // Assign types to captures
    if f.capture_cnt != 0 {
        captured_types.types.iter().enumerate().for_each(|(i, t)| {
            let _ = inferer.update_var(type_mapping.captured(i.into()), &t);
        });
    }

    // User provided function signature
    if let Some(f_type) = &f.type_ {
        // Unify parameters
        for i in 0..f_type.params.len() {
            inferer
                .update_var(type_mapping.local(i.into()), &f_type.params[i])
                .unwrap()
        }
        inferer.update_var(type_mapping.ret(), &f_type.ret).unwrap();

        return (ret_type, assumptions, type_mapping);
    };

    (ret_type, assumptions, type_mapping)
}

fn construct_function_type<'s, 'a>(
    f: &ast::Function<'s>,
    inferer: &mut inferer::Inferer<'s>,
    captured_types: &CapturedTypeInfo<'s>,
    type_mapping: &TypeMapping,
    e: &mut errors::ManyError,
) -> ast::FunctionType<'s> {
    ast::FunctionType {
        params: (0..f.arity)
            .map(|i| {
                inferer
                    .resolve_var(type_mapping.local(i.into()), &f.param_metas[i])
                    .unwrap_or_else(|err| {
                        e.push(err);
                        ast::Type::new_never()
                    })
            })
            .collect(),
        captures: (0..f.capture_cnt)
            .map(|i| {
                inferer
                    .resolve_var(type_mapping.captured(i.into()), &captured_types.metas[i])
                    .unwrap_or_else(|err| {
                        e.push(err);
                        ast::Type::new_never()
                    })
            })
            .collect(),
        ret: Box::new(
            inferer
                .resolve_var(type_mapping.ret(), &f.meta)
                .unwrap_or_else(|err| {
                    e.push(err);
                    ast::Type::new_never()
                }),
        ),
    }
}

fn check_and_resolve_constrains<'s>(
    body: typed::ExprId,
    symbols: &ast::Symbols<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    inferer: &mut inferer::Inferer<'s>,
    assumptions: &constrain::Assumptions<'s>,
    e: &mut errors::ManyError,
) {
    typed_nodes.walk(body, &mut |expr| match expr.node.clone() {
        // Unwrap a unresolved method
        typed::ExprNode::UnresolvedMethod {
            f_name,
            constrains,
            primary_constrain,
            ..
        } => {
            // Try prove priarmary constrain
            match assumptions.which_instance(
                inferer.clone(),
                &primary_constrain,
                &symbols.instances,
                &symbols.relations,
            ) {
                Ok((Some(ins_id), inferer1)) => {
                    // A single hit tells us which instance method implementation
                    expr.node =
                        typed::ExprNode::UserFunction(ast::FunctionId::Method(ins_id, f_name.id()));
                    *inferer = inferer1;
                }
                // The relation is proved to hold, but which instance it is
                // remains unclear
                Ok((None, inferer1)) => *inferer = inferer1,
                Err(constrain::Error::Fail) => e.push(errors::ConstrainProofFailed::new(
                    &expr.meta,
                    &inferer
                        .resolve(&primary_constrain, &primary_constrain.meta)
                        .unwrap_or_else(|_| primary_constrain.clone()),
                )),
                Err(constrain::Error::TooManyHit(hits)) => {
                    let instance_metas = hits
                        .iter()
                        .map(|id| symbols.instances.get(id).unwrap().meta.clone());
                    e.push(errors::TooManyHitMatchingInstance::new(
                        instance_metas,
                        &primary_constrain,
                    ));
                }
                Err(constrain::Error::Resolve(err)) => e.push(err),
            }
            // Try prove each constrain
            for c in constrains.iter() {
                match assumptions.which_instance(
                    inferer.clone(),
                    c,
                    &symbols.instances,
                    &symbols.relations,
                ) {
                    // We don't care which instance makes the constrain hold
                    Ok((_, inferer1)) => *inferer = inferer1,
                    Err(constrain::Error::Fail) => e.push(errors::ConstrainProofFailed::new(
                        &expr.meta,
                        &inferer
                            .resolve(&primary_constrain, &primary_constrain.meta)
                            .unwrap_or_else(|_| primary_constrain.clone()),
                    )),
                    Err(constrain::Error::TooManyHit(hits)) => {
                        let instance_metas = hits
                            .iter()
                            .map(|id| symbols.instances.get(id).unwrap().meta.clone());
                        e.push(errors::TooManyHitMatchingInstance::new(
                            instance_metas,
                            &primary_constrain,
                        ));
                    }
                    Err(constrain::Error::Resolve(err)) => e.push(err),
                }
            }
        }
        _ => {}
    });
}

/// Infer the type a normal function.
pub fn infer_function<'s>(
    id: ast::FunctionId<'s>,
    captured_types: &CapturedTypeInfo<'s>,
    symbols: &ast::Symbols<'s>,
    ast_nodes: &ast::ExprHeap<'s>,
    typed_nodes: &mut typed::ExprHeap<'s>,
    typed_functions: &mut typed::FunctionTable<'s>,
    e: &mut errors::ManyError,
) -> Option<typed::Function<'s>> {
    let f = &symbols.functions[&id];

    if f.arity > u8::MAX as usize {
        e.push(errors::TooManyParameters::new(&f.meta, f.arity));
    }

    let mut inferer = inferer::Inferer::new();

    let assumptions = match &id {
        ast::FunctionId::Method(ins_id, _) => {
            let ins = symbols.instances.get(ins_id).unwrap();
            f.constrains
                .iter()
                .chain(ins.constrains.iter())
                .cloned()
                .collect()
        }
        _ => f.constrains.iter().cloned().collect(),
    };
    let (ret_type, assumptions, type_mapping) =
        init_inferer_for_function_inference(f, &mut inferer, &captured_types, assumptions);
    let assumptions = assumptions.into_iter().collect();

    typed_functions.begin_infering(id.clone());
    // Resolve type of function body
    let body_expr = infer_expr(
        f.body,
        symbols,
        ast_nodes,
        typed_nodes,
        typed_functions,
        &mut inferer,
        &assumptions,
        &type_mapping,
        e,
    );

    inferer
        .unify(&ret_type, &body_expr.type_)
        .map_err(|_| {
            errors::BodyTypeMismatchAnnotation::new(&body_expr.type_, &body_expr.meta, &inferer)
        })
        .commit(e);

    let body = typed_nodes.push(body_expr);

    check_and_resolve_constrains(body, symbols, typed_nodes, &mut inferer, &assumptions, e);

    // Construct type of function
    let f_type = construct_function_type(f, &mut inferer, &captured_types, &type_mapping, e);
    // Discretize type of function such that variables are the first few unsigned integers
    // e.g. from 2, 3 -> 4 to 0, 1 -> 2
    let (map, var_cnt) = inferer.discretization(&f_type);
    let f_type = f_type.substitute_vars_with_nodes(|i| ast::TypeNode::Var(map[&i]));

    typed_functions.finish_infering();

    typed_nodes.walk(body, &mut |expr| {
        macro_rules! get_subst {
            () => {
                |i| {
                    map.get(&i).map_or_else(
                        || {
                            e.push(errors::UnboundTypeVariable::new(&expr.meta));
                            ast::TypeNode::Never
                        },
                        |x| ast::TypeNode::Var(*x),
                    )
                }
            };
        }

        let resolved = inferer
            .resolve(&expr.type_, &expr.meta)
            .unwrap_or_else(|err| {
                e.push(err);
                ast::Type::new_never()
            });

        let substed_type = resolved.substitute_vars_with_nodes(get_subst!());

        if DEBUG {
            let report = miette::miette!(
                labels = vec![miette::LabeledSpan::at(
                    expr.meta.span(),
                    format!("{} = {}", &expr.type_, substed_type)
                ),],
                ""
            )
            .with_source_code(expr.meta.named_source());
            println!("{:?}", report)
        }

        expr.type_ = substed_type;

        match &mut expr.node {
            typed::ExprNode::UnresolvedMethod {
                primary_constrain, ..
            } => {
                let primary_constrain1 = inferer
                    .resolve(primary_constrain, &primary_constrain.meta)
                    .unwrap_or_else(|err| {
                        e.push(err);
                        primary_constrain.clone()
                    });
                *primary_constrain = primary_constrain1.substitute_vars_with_nodes(get_subst!());
            }
            _ => {}
        };
    });

    Some(typed::Function {
        var_cnt,
        body,
        meta: f.meta.clone(),
        type_: f_type,
        capture_cnt: f.capture_cnt,
        local_cnt: f.local_cnt,
    })
}
