use std::collections::HashMap;
use std::collections::VecDeque;

use super::context::GlobalSymbols;
use super::tast;
use super::tast::Path;
use crate::ast;
use crate::ast::TypeApply;
use crate::constrain::Assumptions;
use crate::errors;
use crate::inferer;
use ast::{Meta, Trie};

use super::context::Resolver;

fn check_symbols<'s>(symbols: &ast::Symbols<'s>, e: &mut errors::ManyError) {
    // Check method has been implemented
    symbols.instances.iter().for_each(|(ins_id, ins)| {
        if let Some(rel) = &symbols.relations.get(&ins.rel_name()) {
            rel.f_sigs.keys().for_each(|f_name| {
                let f_id = ast::FunctionId::of_method(ins_id.clone(), *f_name);

                if !symbols.functions.contains_key(&f_id) {
                    e.push(errors::MethodNotImplemented::new(*f_name, &ins.meta))
                }
            });
        }
    });

    // Check relation constrain requirements
    symbols.instances.iter().for_each(|(_, ins)| {
        if let Some(rel) = symbols.relations.get(&ins.rel_name()) {
            let mut inferer = inferer::Inferer::new();
            let _ = inferer.alloc_vars(ins.var_cnt);
            let rel_constrains = rel
                .constrains
                .iter()
                .map(|c| c.substitute_vars(|id| ins.rel.args[id.0].clone()));

            let assumptions: Assumptions = ins.constrains.iter().cloned().collect();

            rel_constrains.for_each(|c| {
                match assumptions.which_instance(
                    inferer.clone(),
                    &c,
                    &symbols.instances,
                    &symbols.relations,
                ) {
                    Ok(_) => (),
                    Err(_) => e.push(errors::RelationConstrainUnsatisfied::new(&c, &ins.meta)),
                }
            });
        }
    });
}

fn lower_types<'s: 'a, 'a>(
    ts: impl Iterator<Item = &'a tast::Type<'s>>,
    ns: &tast::NameSpace<'s>,
    e: &mut errors::ManyError,
    datas: &ast::Trie<ast::DataName<'s>, ast::Data<'s>, &'s str>,
) -> Vec<ast::Type<'s>> {
    ts.map(|t| lower_type(t, ns, e, datas)).collect()
}

fn lower_type<'s>(
    t: &tast::Type<'s>,
    ns: &tast::NameSpace<'s>,
    e: &mut errors::ManyError,
    datas: &ast::Trie<ast::DataName<'s>, ast::Data<'s>, &'s str>,
) -> ast::Type<'s> {
    use tast::TypeNode::*;
    match &t.node {
        Generic(template, args) => match ns.resolve_to_name(template.clone(), datas) {
            Err(err) => {
                err.commit(&template, e);
                ast::Type::new_never()
            }
            Ok(dn) => {
                if args.len() != datas.get(&dn).unwrap().kind_arity {
                    e.push(errors::WrongNumberOfArgs::new(
                        datas.get(&dn).unwrap().kind_arity,
                        &t.meta,
                    ));
                    return ast::Type::new_never();
                }
                let args = lower_types(args.iter(), ns, e, datas);
                ast::Type::new(ast::TypeNode::Generic(dn, args))
            }
        },
        Callable(callable) => ast::Type::new(ast::TypeNode::Callable(lower_callable_type(
            callable, ns, e, &t.meta, datas,
        ))),
        ImpureCallable(callable) => ast::Type::new(ast::TypeNode::ImpureCallable(
            lower_callable_type(callable, ns, e, &t.meta, datas),
        )),
        Tuple(v) => {
            let v = lower_types(v.iter(), ns, e, datas);
            ast::Type::new(ast::TypeNode::Tuple(v))
        }
        Primitive(p) => ast::Type::new(ast::TypeNode::Primitive(p.clone())),
        Var(v) => ast::Type::new_var(*v),
        Never => ast::Type::new_never(),
        Unit => ast::Type::new_unit(),
        WildCard => ast::Type::new_wildcard(),
    }
}

fn lower_callable_type<'s>(
    t: &tast::CallableType<'s>,
    ns: &tast::NameSpace<'s>,
    e: &mut errors::ManyError,
    _meta: &Meta,
    datas: &Trie<ast::DataName<'s>, ast::Data<'s>, &'s str>,
) -> ast::CallableType<'s> {
    let params = lower_types(t.params.iter(), ns, e, datas);
    let ret = lower_type(&t.ret, ns, e, datas);
    ast::CallableType {
        params,
        ret: Box::new(ret),
    }
}

fn lower_builtin_function<'s>(
    f: tast::BuiltinFunction<'s>,
    ns: &tast::NameSpace<'s>,
    e: &mut errors::ManyError,
    datas: &Trie<ast::DataName<'s>, ast::Data<'s>, &'s str>,
) -> ast::BuiltinFunction<'s> {
    let callable = lower_callable_type(&f.type_, ns, e, &f.meta, datas);
    ast::BuiltinFunction {
        var_cnt: f.var_cnt,
        type_: callable,
        meta: f.meta,
        pure: f.pure,
    }
}

fn lower_constructor<'s>(
    c: tast::Constructor<'s>,
    ns: &tast::NameSpace<'s>,
    e: &mut errors::ManyError,
    datas: &Trie<ast::DataName<'s>, ast::Data<'s>, &'s str>,
) -> ast::Constructor<'s> {
    let params = lower_types(c.params.iter(), ns, e, datas);
    ast::Constructor {
        name: c.name,
        params,
        belongs_to: c.belongs_to,
        meta: c.meta,
    }
}

fn lower_constrain<'s, R: tast::RelationArity>(
    c: tast::Constrain<'s>,
    ns: &tast::NameSpace<'s>,
    e: &mut errors::ManyError,
    datas: &Trie<ast::DataName<'s>, ast::Data<'s>, &'s str>,
    relations: &Trie<ast::RelationName<'s>, R, &'s str>,
) -> Result<ast::Constrain<'s>, ()> {
    let rel_name = ns
        .resolve_to_name(c.rel_name.clone(), relations)
        .map_err(|err| {
            err.commit(&c.rel_name, e);
        })?;
    if c.args.len() != relations.get(&rel_name).unwrap().arity() {
        e.push(errors::RelationArityWrong::new(
            &c.meta,
            relations.get(&rel_name).unwrap().arity(),
        ))
    }
    let args = lower_types(c.args.iter(), ns, e, datas);
    Ok(ast::Constrain {
        rel_name,
        args,
        meta: c.meta,
    })
}

fn lower_method_sig<'s, R: tast::RelationArity>(
    sig: tast::MethodSig<'s>,
    ns: &tast::NameSpace<'s>,
    e: &mut errors::ManyError,
    datas: &Trie<ast::DataName<'s>, ast::Data<'s>, &'s str>,
    relations: &Trie<ast::RelationName<'s>, R, &'s str>,
) -> ast::MethodSig<'s> {
    let type_ = lower_callable_type(&sig.type_, ns, e, &sig.meta, datas);
    let constrains = sig
        .constrains
        .into_iter()
        .filter_map(|c| lower_constrain(c, ns, e, datas, relations).ok())
        .collect();
    ast::MethodSig {
        var_cnt: sig.var_cnt,
        type_,
        pure: sig.pure,
        constrains,
        primary_constrain: sig.primary_constrain,
        meta: sig.meta,
    }
}

fn lower_relation<'s, R: tast::RelationArity>(
    r: tast::Relation<'s>,
    ns: &tast::NameSpace<'s>,
    e: &mut errors::ManyError,
    datas: &Trie<ast::DataName<'s>, ast::Data<'s>, &'s str>,
    relations: &Trie<ast::RelationName<'s>, R, &'s str>,
) -> ast::Relation<'s> {
    let constrains = r
        .constrains
        .into_iter()
        .filter_map(|c| lower_constrain(c, ns, e, datas, relations).ok())
        .collect::<Vec<_>>();
    let f_sigs = r
        .f_sigs
        .into_iter()
        .map(|(f_name, sig)| (f_name, lower_method_sig(sig, ns, e, datas, relations)))
        .collect();
    ast::Relation {
        name: r.name.into(),
        dependent: r.dependent,
        constrains,
        arity: r.arity,
        f_sigs,
        meta: r.meta,
    }
}

fn lower_instance<'s, R: tast::RelationArity>(
    ins: tast::Instance<'s>,
    ns: &tast::NameSpace<'s>,
    e: &mut errors::ManyError,
    datas: &Trie<ast::DataName<'s>, ast::Data<'s>, &'s str>,
    relations: &Trie<ast::RelationName<'s>, R, &'s str>,
) -> Result<ast::Instance<'s>, ()> {
    let rel = lower_constrain(ins.rel, ns, e, datas, relations)?;
    let constrains = ins
        .constrains
        .into_iter()
        .filter_map(|c| lower_constrain(c, ns, e, datas, relations).ok())
        .collect();
    Ok(ast::Instance {
        var_cnt: ins.var_cnt,
        rel,
        constrains,
        meta: ins.meta,
    })
}

fn lower_function_id<'s>(
    id: tast::FunctionId<'s>,
    ins_id_map: &HashMap<tast::InstanceId, ast::InstanceId<'s>>,
) -> Option<ast::FunctionId<'s>> {
    match id {
        tast::FunctionId::Method(id, x) => {
            Some(ast::FunctionId::Method(ins_id_map.get(&id)?.clone(), x))
        }
        tast::FunctionId::Standard(p) => Some(ast::FunctionId::Standard(p)),
    }
}

pub fn lower_symbols<'s>(
    tast_symbols: tast::Symbols<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::Symbols<'s> {
    let module_namespaces = tast_symbols
        .module_namespaces
        .iter()
        .map(|(mp, ops)| {
            let ns = tast::NameSpace::new(mp.clone());
            let ns = ops.into_iter().fold(ns, |ns, op| {
                ns.applied(
                    op.clone(),
                    &|p| {
                        tast_symbols.builtins.contains_path(p.iter())
                            || tast_symbols.constructors.contains_path(p.iter())
                            || tast_symbols.datas.contains_path(p.iter())
                            || tast_symbols.methods.contains_path(p.iter())
                            || tast_symbols.relations.contains_path(p.iter())
                            || tast_symbols.module_namespaces.contains_path(p.iter())
                            || tast_symbols.functions.constains_standard_function(p.iter())
                    },
                    e,
                )
            });
            (mp, ns)
        })
        .collect::<Trie<_, _, _>>();

    let constructors = tast_symbols
        .constructors
        .into_iter()
        .map(|(cn, c)| {
            let ns = module_namespaces
                .get_by_path(cn.module_path().iter())
                .unwrap();
            (cn, lower_constructor(c, ns, e, &tast_symbols.datas))
        })
        .collect::<Trie<_, _, _>>();

    let builtins = tast_symbols
        .builtins
        .into_iter()
        .map(|(bn, b)| {
            let ns = module_namespaces
                .get_by_path(bn.module_path().iter())
                .unwrap();
            (bn, lower_builtin_function(b, ns, e, &tast_symbols.datas))
        })
        .collect::<Trie<_, _, _>>();

    let relations = tast_symbols
        .relations
        .iter()
        .map(|(rn, r)| {
            let ns = module_namespaces
                .get_by_path(rn.module_path().iter())
                .unwrap();
            (
                rn.clone(),
                lower_relation(
                    r.clone(),
                    ns,
                    e,
                    &tast_symbols.datas,
                    &tast_symbols.relations,
                ),
            )
        })
        .collect::<Trie<_, _, _>>();

    let standard_function_names = tast_symbols
        .functions
        .iter()
        .filter_map(|(id, _)| match id {
            tast::FunctionId::Standard(p) => Some((ast::FunctionName::from(p.clone()), ())),
            _ => None
        })
        .collect::<Trie<_, _, _>>();

    let mut instance_id_map = HashMap::new();
    let instances = tast_symbols.instances.into_iter().fold(
        ast::InstanceIdTable::new(),
        |mut tab, (p, i, ins)| {
            let ns = module_namespaces.get_by_path(p.iter()).unwrap();
            if let Ok(ins) =
                lower_instance(ins, ns, e, &tast_symbols.datas, &tast_symbols.relations)
            {
                let rel_name = ins.rel_name();
                let ins_id = tab.insert(rel_name.clone(), ins);
                instance_id_map.insert(i, ins_id);
            }
            tab
        },
    );

    // Lower each function
    let mut functions = ast::FunctionIdTable::new();
    for (fid, f) in tast_symbols.functions.into_iter() {
        if let Some(fid) = lower_function_id(fid, &instance_id_map) {
            // Some checks for methods
            if let ast::FunctionId::Method(ref ins_id, ref x) = fid {
                // Check if method exists
                if let Some(rel) = tast_symbols.relations.get(&ins_id.rel_name) {
                    if !rel.f_sigs.contains_key(x) {
                        e.push(errors::UndeclaredMethod::new(&f.meta));
                        continue;
                    }
                }

                // TODO Check metdho type and constrain correct
            }
            let ns = match &fid {
                ast::FunctionId::Method(ins_id, _) => module_namespaces
                    .get_by_path(instances.module_for(ins_id).iter())
                    .unwrap(),
                ast::FunctionId::Standard(p) => {
                    module_namespaces.get_by_path(p.head().iter()).unwrap()
                }
                _ => unreachable!(),
            };

            // Good to lower
            let resolver_symbols = GlobalSymbols {
                builtins: &builtins,
                functions: &standard_function_names,
                constructors: &constructors,
                relations: &relations,
                datas: &tast_symbols.datas,
                modules: &module_namespaces,
                methods: &tast_symbols.methods,
            };
            if let Ok(ast_f) =
                lower_function(f, ns.clone(), resolver_symbols, &mut functions, ast_heap, e)
            {
                functions.insert(fid, ast_f);
            }
        }
    }

    // Assign each method with required signature
    functions.iter_mut().for_each(|(fid, f)| {
        if let ast::FunctionId::Method(ins_id, x) = fid {
            let rel = relations.get(&ins_id.rel_name).unwrap();
            let ins = instances.get(&ins_id).unwrap();

            let f_sig = rel.f_sigs[x].substitute_vars(|id| {
                ins.rel
                    .args
                    .get(id.0)
                    .cloned()
                    .unwrap_or_else(|| ast::Type::new_var(id))
            });

            if f.type_.is_some() {
                e.push(errors::MethodTypeAnnotationNotSupported::new(&f.meta));
            }
            f.type_ = Some(f_sig.type_);
            f.constrains = f_sig.constrains;
            f.var_cnt = f_sig.var_cnt;
        }
    });

    let ast_symbols = ast::Symbols {
        functions,
        constructors,
        datas: tast_symbols.datas,
        builtins,
        relations,
        instances,
        methods: tast_symbols.methods,
    };

    check_symbols(&ast_symbols, e);
    ast_symbols
}

fn lower_function<'s, 'sy, B, F, C, R: tast::RelationArity>(
    f: tast::Function<'s>,
    ns: tast::NameSpace<'s>,
    sy: GlobalSymbols<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> Result<ast::Function<'s>, ()> {
    let mut resolver = Resolver::new(ns, sy);

    let arity = f.params.len();

    let (local_cnt, body) = resolver.with_function_scope(|resolver| {
        resolver.with_scope(|resolver| {
            f.params.into_iter().for_each(|p| {
                resolver.define_local(p, !f.pure);
            });
            let body = lower_expr(*f.body, resolver, functions, ast_heap, e);
            body
        })
    });

    let type_ = f
        .type_
        .map(|t| lower_callable_type(&t, &resolver.global, e, &f.meta, resolver.symbols.datas));

    let constrains = f
        .constrains
        .into_iter()
        .filter_map(|c| {
            lower_constrain(
                c,
                &resolver.global,
                e,
                resolver.symbols.datas,
                resolver.symbols.relations,
            )
            .ok()
        })
        .collect();

    let ast_f = ast::Function {
        type_,
        var_cnt: f.var_cnt,
        local_cnt,
        arity,
        body,
        meta: f.meta,
        param_metas: f.param_metas,
        captures: Vec::new(),
        captures_meta: Vec::new(),
        pure: f.pure,
        constrains,
    };
    Ok(ast_f)
}

fn lower_constant<'s>(
    c: &tast::Constant<'s>,
    meta: &Meta,
    e: &mut errors::ManyError,
) -> ast::Constant<'s> {
    use ast::Constant::*;
    use tast::Constant;
    match c {
        Constant::Bool(b) => Bool(*b),
        Constant::Char(c) => match c.chars().next() {
            Some(ch) if c.len() == ch.len_utf8() => Char(ch),
            _ => {
                e.push(errors::NotAChar::new(meta));
                Char('\0')
            }
        },
        Constant::Float(f) => Float(*f),
        Constant::Int(i) => Int(*i),
        Constant::Str(s) => Str(s.clone()),
    }
}

fn lower_expr<'s, 'sy, B, F, C, R: tast::RelationArity>(
    expr: tast::Expr<'s>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    use tast::ExprNode::*;
    let type_ = expr.type_.map(|t| {
        (
            lower_type(&t, &resolver.global, e, resolver.symbols.datas),
            t.meta,
        )
    });
    match expr.node {
        Apply { callee, args } => lower_apply(
            *callee, args, expr.meta, type_, resolver, functions, ast_heap, e,
        ),
        IfElse { test, then, else_ } => lower_if_else(
            *test, *then, *else_, expr.meta, type_, resolver, functions, ast_heap, e,
        ),
        Case { operand, arms } => lower_case_of(
            *operand, arms, expr.meta, type_, resolver, functions, ast_heap, e,
        ),
        LetPatIn { bind, value, in_ } => lower_let_pat(
            bind, *value, *in_, expr.meta, type_, resolver, functions, ast_heap, e,
        ),
        LetFnIn { identifier, f, in_ } => lower_let_fn(
            identifier, f, *in_, expr.meta, type_, resolver, functions, ast_heap, e,
        ),
        Tuple(args) => lower_tuple(args, expr.meta, type_, resolver, functions, ast_heap, e),
        Constant(c) => {
            let expr = ast::Expr::new(
                ast::ExprNode::Constant(lower_constant(&c, &expr.meta, e)),
                expr.meta,
                type_,
            );
            ast_heap.push(expr)
        }
        Identifier(id) => lower_identifier(id, expr.meta, resolver, ast_heap, e),
        Seq(stmts, result) => lower_seq(
            stmts.into(),
            result,
            expr.meta,
            resolver,
            functions,
            ast_heap,
            e,
        ),
        Assign(to, from) => lower_assign(*to, *from, expr.meta, resolver, functions, ast_heap, e),
        Access(lhs, rhs) => lower_access(
            *lhs, *rhs, expr.meta, type_, resolver, functions, ast_heap, e,
        ),
        InNameSpace(ns_ops, expr) => {
            lower_in_namespace(ns_ops, *expr, resolver, functions, ast_heap, e)
        }
        Unit => ast_heap.push(ast::Expr::new_untyped(ast::ExprNode::Unit, expr.meta)),
    }
}

fn lower_in_namespace<'s, 'sy, B, F, C, R: tast::RelationArity>(
    ns_ops: Vec<tast::NameSpaceOp<'s>>,
    expr: tast::Expr<'s>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let ns_old = resolver.global.clone();
    ns_ops.into_iter().for_each(|op| {
        resolver.global.apply(
            op,
            &|p| {
                resolver.symbols.contains(p.iter())
            },
            e,
        )
    });
    let expr_id = lower_expr(expr, resolver, functions, ast_heap, e);
    resolver.global = ns_old;
    expr_id
}

fn reduce_to_path<'s>(
    lhs: tast::Expr<'s>,
    rhs: tast::Expr<'s>,
    meta: Meta,
) -> Result<tast::Path<'s>, errors::BadAccessOperands> {
    match rhs.node {
        tast::ExprNode::Identifier(rhs) => match lhs.node {
            tast::ExprNode::Identifier(lhs) => Ok(tast::Path {
                path: vec![lhs, rhs],
                meta,
            }),
            tast::ExprNode::Access(llhs, lrhs) => {
                let tast::Path { mut path, .. } = reduce_to_path(*llhs, *lrhs, lhs.meta)?;
                path.insert(0, rhs);
                Ok(tast::Path { path, meta })
            }
            _ => Err(errors::BadAccessOperands::new(&meta)),
        },
        _ => Err(errors::BadAccessOperands::new(&meta)),
    }
}

fn lower_access<'s, 'sy, B, F, C, R: tast::RelationArity>(
    lhs: tast::Expr<'s>,
    rhs: tast::Expr<'s>,
    meta: Meta,
    type_: Option<(ast::Type<'s>, Meta)>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    _functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    match reduce_to_path(lhs, rhs, meta.clone()) {
        Err(err) => {
            e.push(err);
            ast_heap.push(ast::Expr::new(ast::ExprNode::Never, meta, type_))
        }
        Ok(p) => {
            let expr = resolver.resolve_global(p, e, meta);
            ast_heap.push(expr)
        }
    }
}

fn lower_assign<'s, 'sy, B, F, C, R: tast::RelationArity>(
    to: tast::Expr<'s>,
    from: tast::Expr<'s>,
    meta: Meta,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let to = lower_expr(to, resolver, functions, ast_heap, e);
    let from = lower_expr(from, resolver, functions, ast_heap, e);
    let expr = ast::Expr::new_untyped(ast::ExprNode::Assign(from, to), meta);
    ast_heap.push(expr)
}

fn lower_stmt<'s, 'sy, B, F, C, R: tast::RelationArity>(
    stmt: tast::Stmt<'s>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::Stmt {
    use tast::StmtNode::*;
    match stmt.node {
        LetDecl(..) => unreachable!(),
        If { test, then } => {
            let test = lower_expr(test, resolver, functions, ast_heap, e);
            let then = lower_expr(then, resolver, functions, ast_heap, e);
            ast::Stmt::new(ast::StmtNode::If { test, then }, stmt.meta)
        }
        While { test, then } => {
            let test = lower_expr(test, resolver, functions, ast_heap, e);
            let then = lower_expr(then, resolver, functions, ast_heap, e);
            ast::Stmt::new(ast::StmtNode::While { test, then }, stmt.meta)
        }
        Expr(expr) => {
            let expr = lower_expr(expr, resolver, functions, ast_heap, e);
            ast::Stmt::new(ast::StmtNode::Expr(expr), stmt.meta)
        }
    }
}

fn lower_seq<'s, 'sy, B, F, C, R: tast::RelationArity>(
    mut stmts: VecDeque<tast::Stmt<'s>>,
    result: Option<Box<tast::Expr<'s>>>,
    result_meta: ast::Meta,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    resolver.with_scope(|resolver| match stmts.pop_front() {
        Some(tast::Stmt {
            node: tast::StmtNode::LetDecl(pat, value),
            meta,
        }) => {
            let expr = lower_pattern(pat, resolver, e).map_or_else(
                |_| ast::Expr::new_never(meta.clone()),
                |pat| {
                    let rest =
                        lower_seq(stmts, result, result_meta, resolver, functions, ast_heap, e);
                    let value = lower_expr(value, resolver, functions, ast_heap, e);
                    ast::Expr::new_untyped(
                        ast::ExprNode::LetPatIn {
                            bind: pat,
                            value,
                            in_: rest,
                        },
                        meta.clone(),
                    )
                },
            );
            ast_heap.push(expr)
        }
        Some(stmt) => {
            let rest = lower_seq(
                stmts,
                result,
                result_meta.clone(),
                resolver,
                functions,
                ast_heap,
                e,
            );
            let stmt = lower_stmt(stmt, resolver, functions, ast_heap, e);
            match &mut ast_heap[rest] {
                ast::Expr {
                    node: ast::ExprNode::Seq(stmts, ..),
                    ..
                } => {
                    stmts.push_front(stmt);
                    rest
                }
                _ => {
                    let seq = VecDeque::from([stmt]);
                    let expr =
                        ast::Expr::new_untyped(ast::ExprNode::Seq(seq, Some(rest)), result_meta);
                    ast_heap.push(expr)
                }
            }
        }
        None => {
            let expr = ast::Expr::new_untyped(
                ast::ExprNode::Seq(
                    VecDeque::new(),
                    result.map(|expr| lower_expr(*expr, resolver, functions, ast_heap, e)),
                ),
                result_meta,
            );
            ast_heap.push(expr)
        }
    })
}

fn lower_apply<'s, 'sy, B, F, C, R: tast::RelationArity>(
    callee: tast::Expr<'s>,
    mut args: Vec<tast::Expr<'s>>,
    apply_meta: Meta,
    type_: Option<(ast::Type<'s>, Meta)>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    if args.len() == 1 && matches!(&args[0].node, tast::ExprNode::Unit) {
        args.pop().unwrap();
    }

    let callee = lower_expr(callee, resolver, functions, ast_heap, e);
    let args = args
        .into_iter()
        .map(|arg| lower_expr(arg, resolver, functions, ast_heap, e))
        .collect::<Vec<_>>();
    ast_heap.push(ast::Expr::new(
        ast::ExprNode::Apply { callee, args },
        apply_meta,
        type_,
    ))
}

fn lower_if_else<'s, 'sy, B, F, C, R: tast::RelationArity>(
    test: tast::Expr<'s>,
    then: tast::Expr<'s>,
    else_: tast::Expr<'s>,
    if_else_meta: Meta,
    type_: Option<(ast::Type<'s>, Meta)>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let test = lower_expr(test, resolver, functions, ast_heap, e);
    let then = lower_expr(then, resolver, functions, ast_heap, e);
    let else_ = lower_expr(else_, resolver, functions, ast_heap, e);
    ast_heap.push(ast::Expr::new(
        ast::ExprNode::IfElse { test, then, else_ },
        if_else_meta.clone(),
        type_.clone(),
    ))
}

fn lower_pattern<'s, 'sy, B, F, C, R: tast::RelationArity>(
    pat: tast::Pattern<'s>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    e: &mut errors::ManyError,
) -> Result<ast::Pattern<'s>, ()> {
    use tast::Pattern::*;
    let r = match pat {
        Construct(template, args, meta) => ast::Pattern::Construct(
            resolver
                .global
                .resolve_to_name(template.clone(), resolver.symbols.constructors)
                .map_err(|err| err.commit(&template, e))?,
            {
                let mut args_lowered = Vec::new();
                for arg in args.into_iter() {
                    args_lowered.push(lower_pattern(arg, resolver, e)?);
                }
                args_lowered
            },
            meta,
        ),
        Bind(id, mutable, meta) => ast::Pattern::Bind(resolver.define_local(id, mutable), meta),
        Literal(constant, meta) => ast::Pattern::Literal(lower_constant(&constant, &meta, e), meta),
        Tuple(args, meta) => ast::Pattern::Tuple(
            {
                let mut args_lowered = Vec::new();
                for arg in args.into_iter() {
                    args_lowered.push(lower_pattern(arg, resolver, e)?);
                }
                args_lowered
            },
            meta,
        ),
    };
    Ok(r)
}

fn lower_let_pat<'s, 'sy, B, F, C, R: tast::RelationArity>(
    pat: tast::Pattern<'s>,
    value: tast::Expr<'s>,
    in_: tast::Expr<'s>,
    let_pat_meta: Meta,
    type_: Option<(ast::Type<'s>, Meta)>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let value = lower_expr(value, resolver, functions, ast_heap, e);
    resolver.with_scope(|resolver| {
        let expr = lower_pattern(pat, resolver, e).map_or_else(
            |_| ast::Expr::new_never(let_pat_meta.clone()),
            |pat| {
                let in_ = lower_expr(in_, resolver, functions, ast_heap, e);

                ast::Expr::new(
                    ast::ExprNode::LetPatIn {
                        bind: pat,
                        value,
                        in_,
                    },
                    let_pat_meta.clone(),
                    type_,
                )
            },
        );
        ast_heap.push(expr)
    })
}

fn lower_case_of<'s, 'sy, B, F, C, R: tast::RelationArity>(
    operand: tast::Expr<'s>,
    arms: Vec<tast::CaseArm<'s>>,
    case_meta: Meta,
    type_: Option<(ast::Type<'s>, Meta)>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let operand = lower_expr(operand, resolver, functions, ast_heap, e);
    let arms = arms
        .into_iter()
        .filter_map(|arm| {
            resolver.with_scope(|resolver| {
                let pat = lower_pattern(arm.pattern, resolver, e).ok()?;
                let guard = arm
                    .guard
                    .map(|g| lower_expr(g, resolver, functions, ast_heap, e));
                let result = lower_expr(arm.result, resolver, functions, ast_heap, e);
                Some(ast::CaseArm {
                    pattern: pat,
                    guard,
                    result,
                })
            })
        })
        .collect();
    ast_heap.push(ast::Expr::new(
        ast::ExprNode::Case { operand, arms },
        case_meta,
        type_,
    ))
}

fn lower_let_fn<'s, 'sy, B, F, C, R: tast::RelationArity>(
    id: &'s str,
    f: tast::Function<'s>,
    in_: tast::Expr<'s>,
    let_fn_meta: Meta,
    type_: Option<(ast::Type<'s>, Meta)>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let r = resolver.with_scope(|resolver| {
        let local_id = resolver.define_local(id, false);

        let (captures, captures_meta, local_cnt, body) =
            resolver.with_closure_scope(id, |resolver| {
                f.params.iter().for_each(|p| {
                    resolver.define_local(p, !f.pure);
                });
                lower_expr(*f.body, resolver, functions, ast_heap, e)
            });

        let constrains = f
            .constrains
            .into_iter()
            .filter_map(|c| {
                lower_constrain(
                    c,
                    &resolver.global,
                    e,
                    resolver.symbols.datas,
                    resolver.symbols.relations,
                )
                .ok()
            })
            .collect();

        let ast_f = ast::Function {
            type_: None,
            var_cnt: 0,
            local_cnt,
            arity: f.params.len(),
            body,
            meta: f.meta.clone(),
            param_metas: f.param_metas,
            captures,
            captures_meta,
            pure: f.pure,
            constrains,
        };
        let closure_fid = ast::FunctionId::of_closure_at(&f.meta);
        functions.insert(closure_fid.clone(), ast_f);

        let closure_expr = ast_heap.push(ast::Expr::new_untyped(
            ast::ExprNode::MakeClosure(closure_fid),
            f.meta.clone(),
        ));

        let in_ = lower_expr(in_, resolver, functions, ast_heap, e);
        ast::Expr::new(
            ast::ExprNode::LetIn {
                bind: local_id,
                value: closure_expr,
                in_,
            },
            let_fn_meta,
            type_,
        )
    });
    ast_heap.push(r)
}

fn lower_tuple<'s, 'sy, B, F, C, R: tast::RelationArity>(
    args: Vec<tast::Expr<'s>>,
    tuple_meta: Meta,
    type_: Option<(ast::Type<'s>, Meta)>,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    functions: &mut ast::FunctionTable<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let args = args
        .into_iter()
        .map(|arg| lower_expr(arg, resolver, functions, ast_heap, e))
        .collect();
    ast_heap.push(ast::Expr::new(
        ast::ExprNode::Tuple(args),
        tuple_meta,
        type_,
    ))
}

fn lower_identifier<'s, 'sy, B, F, C, R: tast::RelationArity>(
    id: &'s str,
    id_meta: Meta,
    resolver: &mut Resolver<'s, 'sy, B, F, C, ast::Data<'s>, R>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    if let Some(resolved) = resolver.resolve(id, &id_meta) {
        ast_heap.push(resolved)
    } else {
        ast_heap.push(resolver.resolve_global(Path::skeleton(id, id_meta.clone()), e, id_meta))
    }
}
