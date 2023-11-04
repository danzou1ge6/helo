use std::collections::VecDeque;

use super::tast;
use crate::ast;
use crate::errors;
use ast::Meta;

use super::context::Resolver;

pub fn lower_symbols<'s>(
    tast_symbols: tast::Symbols<'s>,
    symbols: &mut ast::Symbols<'s>,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) {
    for (fid, f) in tast_symbols.functions.into_iter() {
        let ast_f = lower_function(f, symbols, ast_heap, e);
        symbols.add_function(fid, ast_f);
    }

    for (data_name, data) in tast_symbols.datas.into_iter() {
        if data.constructors.len() > u8::MAX as usize {
            e.push(errors::TooManyVariants::new(
                &data.meta,
                data.constructors.len(),
            ))
        }
        symbols.add_data(data_name, data);
    }

    for (constructor_name, constructor) in tast_symbols.constructors.into_iter() {
        if !constructor_name.chars().next().unwrap().is_uppercase() {
            e.push(errors::ConstructorNameNotUppercase::new(&constructor.meta));
        }
        symbols.add_constructor(constructor_name, constructor);
    }
}

fn lower_function<'s>(
    f: tast::Function<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::Function<'s> {
    let mut resolver = Resolver::new();

    let arity = f.params.len();

    let (local_cnt, body) = resolver.with_function_scope(|resolver| {
        resolver.with_scope(|resolver| {
            f.params.into_iter().for_each(|p| {
                resolver.define_local(p, !f.pure);
            });
            let body = lower_expr(*f.body, resolver, symbols, ast_heap, e);
            body
        })
    });

    let ast_f = ast::Function {
        type_: f.type_,
        var_cnt: f.var_cnt,
        local_cnt,
        arity,
        body,
        meta: f.meta,
        param_metas: f.param_metas,
        captures: Vec::new(),
        captures_meta: Vec::new(),
        pure: f.pure,
    };
    ast_f
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

fn lower_expr<'s>(
    expr: tast::Expr<'s>,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    use tast::ExprNode::*;
    match expr.node {
        Apply { callee, args } => lower_apply(
            *callee, args, expr.meta, expr.type_, resolver, symbols, ast_heap, e,
        ),
        IfElse { test, then, else_ } => lower_if_else(
            *test, *then, *else_, expr.meta, expr.type_, resolver, symbols, ast_heap, e,
        ),
        Case { operand, arms } => lower_case_of(
            *operand, arms, expr.meta, expr.type_, resolver, symbols, ast_heap, e,
        ),
        LetPatIn { bind, value, in_ } => lower_let_pat(
            bind, *value, *in_, expr.meta, expr.type_, resolver, symbols, ast_heap, e,
        ),
        LetFnIn { identifier, f, in_ } => lower_let_fn(
            identifier, f, *in_, expr.meta, expr.type_, resolver, symbols, ast_heap, e,
        ),
        Tuple(args) => lower_tuple(args, expr.meta, expr.type_, resolver, symbols, ast_heap, e),
        Constant(c) => {
            let expr = ast::Expr::new(
                ast::ExprNode::Constant(lower_constant(&c, &expr.meta, e)),
                expr.meta,
                expr.type_,
            );
            ast_heap.push(expr)
        }
        Identifier(id) => {
            lower_identifier(id, expr.meta, expr.type_, resolver, symbols, ast_heap, e)
        }
        Seq(stmts, result) => lower_seq(
            stmts.into(),
            result,
            expr.meta,
            resolver,
            symbols,
            ast_heap,
            e,
        ),
        Assign(to, from) => lower_assign(*to, *from, expr.meta, resolver, symbols, ast_heap, e),
        Unit => ast_heap.push(ast::Expr::new_untyped(ast::ExprNode::Unit, expr.meta)),
    }
}

fn lower_assign<'s>(
    to: tast::Expr<'s>,
    from: tast::Expr<'s>,
    meta: Meta,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let to = lower_expr(to, resolver, symbols, ast_heap, e);
    let from = lower_expr(from, resolver, symbols, ast_heap, e);
    let expr = ast::Expr::new_untyped(ast::ExprNode::Assign(from, to), meta);
    ast_heap.push(expr)
}

fn lower_stmt<'s>(
    stmt: tast::Stmt<'s>,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::Stmt {
    use tast::StmtNode::*;
    match stmt.node {
        LetDecl(..) => unreachable!(),
        If { test, then } => {
            let test = lower_expr(test, resolver, symbols, ast_heap, e);
            let then = lower_expr(then, resolver, symbols, ast_heap, e);
            ast::Stmt::new(ast::StmtNode::If { test, then }, stmt.meta)
        }
        While { test, then } => {
            let test = lower_expr(test, resolver, symbols, ast_heap, e);
            let then = lower_expr(then, resolver, symbols, ast_heap, e);
            ast::Stmt::new(ast::StmtNode::While { test, then }, stmt.meta)
        }
        Expr(expr) => {
            let expr = lower_expr(expr, resolver, symbols, ast_heap, e);
            ast::Stmt::new(ast::StmtNode::Expr(expr), stmt.meta)
        }
    }
}

fn lower_seq<'s>(
    mut stmts: VecDeque<tast::Stmt<'s>>,
    result: Option<Box<tast::Expr<'s>>>,
    result_meta: ast::Meta,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    resolver.with_scope(|resolver| match stmts.pop_front() {
        Some(tast::Stmt {
            node: tast::StmtNode::LetDecl(pat, value),
            meta,
        }) => {
            let pat = lower_pattern(pat, resolver, e);
            let rest = lower_seq(stmts, result, result_meta, resolver, symbols, ast_heap, e);
            let value = lower_expr(value, resolver, symbols, ast_heap, e);
            let expr = ast::Expr::new_untyped(
                ast::ExprNode::LetPatIn {
                    bind: pat,
                    value,
                    in_: rest,
                },
                meta,
            );
            ast_heap.push(expr)
        }
        Some(stmt) => {
            let rest = lower_seq(stmts, result, result_meta, resolver, symbols, ast_heap, e);
            let stmt = lower_stmt(stmt, resolver, symbols, ast_heap, e);
            match &mut ast_heap[rest] {
                ast::Expr {
                    node: ast::ExprNode::Seq(stmts, ..),
                    ..
                } => stmts.push_front(stmt),
                _ => unreachable!(),
            };
            rest
        }
        None => {
            let expr = ast::Expr::new_untyped(
                ast::ExprNode::Seq(
                    VecDeque::new(),
                    result.map(|expr| lower_expr(*expr, resolver, symbols, ast_heap, e)),
                ),
                result_meta,
            );
            ast_heap.push(expr)
        }
    })
}

fn lower_apply<'s>(
    callee: tast::Expr<'s>,
    mut args: Vec<tast::Expr<'s>>,
    apply_meta: Meta,
    type_: Option<ast::Type<'s>>,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    if args.len() == 1 && matches!(&args[0].node, tast::ExprNode::Unit) {
        args.pop().unwrap();
    }

    let callee = lower_expr(callee, resolver, symbols, ast_heap, e);
    let args = args
        .into_iter()
        .map(|arg| lower_expr(arg, resolver, symbols, ast_heap, e))
        .collect::<Vec<_>>();
    ast_heap.push(ast::Expr::new(
        ast::ExprNode::Apply { callee, args },
        apply_meta,
        type_,
    ))
}

fn lower_if_else<'s>(
    test: tast::Expr<'s>,
    then: tast::Expr<'s>,
    else_: tast::Expr<'s>,
    if_else_meta: Meta,
    type_: Option<ast::Type<'s>>,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let test = lower_expr(test, resolver, symbols, ast_heap, e);
    let then = lower_expr(then, resolver, symbols, ast_heap, e);
    let else_ = lower_expr(else_, resolver, symbols, ast_heap, e);
    ast_heap.push(ast::Expr::new(
        ast::ExprNode::IfElse { test, then, else_ },
        if_else_meta.clone(),
        type_.clone(),
    ))
}

fn lower_pattern<'s>(
    pat: tast::Pattern<'s>,
    resolver: &mut Resolver<'s>,
    e: &mut errors::ManyError,
) -> ast::Pattern<'s> {
    use tast::Pattern::*;
    match pat {
        Construct(template, args, meta) => ast::Pattern::Construct(
            &template,
            args.into_iter()
                .map(|p| lower_pattern(p, resolver, e))
                .collect(),
            meta,
        ),
        Bind(id, mutable, meta) => ast::Pattern::Bind(resolver.define_local(id, mutable), meta),
        Literal(constant, meta) => ast::Pattern::Literal(lower_constant(&constant, &meta, e), meta),
        Tuple(args, meta) => ast::Pattern::Tuple(
            args.into_iter()
                .map(|p| lower_pattern(p, resolver, e))
                .collect(),
            meta,
        ),
    }
}

fn lower_let_pat<'s>(
    pat: tast::Pattern<'s>,
    value: tast::Expr<'s>,
    in_: tast::Expr<'s>,
    let_pat_meta: Meta,
    type_: Option<ast::Type<'s>>,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let value = lower_expr(value, resolver, symbols, ast_heap, e);
    resolver.with_scope(|resolver| {
        let pat = lower_pattern(pat, resolver, e);
        let in_ = lower_expr(in_, resolver, symbols, ast_heap, e);
        ast_heap.push(ast::Expr::new(
            ast::ExprNode::LetPatIn {
                bind: pat,
                value,
                in_,
            },
            let_pat_meta,
            type_,
        ))
    })
}

fn lower_case_of<'s>(
    operand: tast::Expr<'s>,
    arms: Vec<tast::CaseArm<'s>>,
    case_meta: Meta,
    type_: Option<ast::Type<'s>>,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let operand = lower_expr(operand, resolver, symbols, ast_heap, e);
    let arms = arms
        .into_iter()
        .map(|arm| {
            resolver.with_scope(|resolver| {
                let pat = lower_pattern(arm.pattern, resolver, e);
                let guard = arm
                    .guard
                    .map(|g| lower_expr(g, resolver, symbols, ast_heap, e));
                let result = lower_expr(arm.result, resolver, symbols, ast_heap, e);
                ast::CaseArm {
                    pattern: pat,
                    guard,
                    result,
                }
            })
        })
        .collect();
    ast_heap.push(ast::Expr::new(
        ast::ExprNode::Case { operand, arms },
        case_meta,
        type_,
    ))
}

fn lower_let_fn<'s>(
    id: &'s str,
    f: tast::Function<'s>,
    in_: tast::Expr<'s>,
    let_fn_meta: Meta,
    type_: Option<ast::Type<'s>>,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
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
                lower_expr(*f.body, resolver, symbols, ast_heap, e)
            });

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
        };
        let closure_fid = f.meta.closure_id();
        symbols.add_function(closure_fid.clone(), ast_f);

        let closure_expr = ast_heap.push(ast::Expr::new_untyped(
            ast::ExprNode::MakeClosure(closure_fid),
            f.meta.clone(),
        ));

        let in_ = lower_expr(in_, resolver, symbols, ast_heap, e);
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

fn lower_tuple<'s>(
    args: Vec<tast::Expr<'s>>,
    tuple_meta: Meta,
    type_: Option<ast::Type<'s>>,
    resolver: &mut Resolver<'s>,
    symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
) -> ast::ExprId {
    let args = args
        .into_iter()
        .map(|arg| lower_expr(arg, resolver, symbols, ast_heap, e))
        .collect();
    ast_heap.push(ast::Expr::new(
        ast::ExprNode::Tuple(args),
        tuple_meta,
        type_,
    ))
}

fn lower_identifier<'s>(
    id: &'s str,
    id_meta: Meta,
    type_: Option<ast::Type<'s>>,
    resolver: &mut Resolver<'s>,
    _symbols: &mut ast::Symbols,
    ast_heap: &mut ast::ExprHeap<'s>,
    _e: &mut errors::ManyError,
) -> ast::ExprId {
    if let Some(resolved) = resolver.resolve(id, &id_meta) {
        ast_heap.push(resolved)
    } else {
        ast_heap.push(ast::Expr::new(ast::ExprNode::Global(id), id_meta, type_))
    }
}
