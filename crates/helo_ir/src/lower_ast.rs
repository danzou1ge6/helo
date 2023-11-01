use crate::ir;
use helo_parse::ast;
use helo_parse::typed;

pub struct Context {
    /// Count of ir locals, NOT ast locals
    local_cnt: usize,
    captured_cnt: usize,
}

impl Context {
    fn new_local(&mut self) -> ir::LocalId {
        let r = ir::LocalId::from(self.local_cnt);
        self.local_cnt += 1;
        r
    }

    fn with_new_function<R>(
        &mut self,
        local_cnt: usize,
        captured_cnt: usize,
        f: impl FnOnce(&mut Context) -> R,
    ) -> R {
        self.local_cnt = local_cnt + captured_cnt;
        self.captured_cnt = captured_cnt;
        f(self)
    }

    pub fn new() -> Self {
        Self {
            local_cnt: 0,
            captured_cnt: 0,
        }
    }
}

pub fn lower_function<'s>(
    fid: &ast::FunctionId,
    symbols: &typed::Symbols<'s>,
    typed_nodes: &typed::ExprHeap<'s>,
    ir_nodes: &mut ir::ExprHeap<'s>,
    str_table: &mut ir::StrTable,
) -> ir::Function {
    let f = symbols.function(fid);
    let mut lower_ctx = Context::new();
    let f_name_id = str_table.add(fid.clone());
    lower_ctx.with_new_function(f.local_cnt, f.captures.len(), |lower_ctx| {
        let body = lower_expr(f.body, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
        ir::Function {
            local_cnt: lower_ctx.local_cnt,
            arity: f.type_.params.len() + f.type_.captures.len(),
            body,
            meta: f.meta.clone(),
            name: f_name_id,
            has_return: !f.type_.ret.node.is_unit(),
        }
    })
}

fn lower_stmt<'s>(
    stmt: &typed::Stmt,
    symbols: &typed::Symbols<'s>,
    typed_nodes: &typed::ExprHeap<'s>,
    ir_nodes: &mut ir::ExprHeap<'s>,
    str_table: &mut ir::StrTable,
    lower_ctx: &mut Context,
) -> ir::ExprId {
    match &stmt.node {
        typed::StmtNode::If { test, then } => {
            let test = lower_expr(*test, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
            let then = lower_expr(*then, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
            ir_nodes.push(ir::Expr::new(
                ir::ExprNode::While { test, then },
                (&stmt.meta).into(),
            ))
        }

        typed::StmtNode::While { test, then } => {
            let test = lower_expr(*test, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
            let then = lower_expr(*then, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
            ir_nodes.push(ir::Expr::new(
                ir::ExprNode::While { test, then },
                (&stmt.meta).into(),
            ))
        }
        typed::StmtNode::Expr(expr) => {
            let expr = lower_expr(*expr, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
            expr
        }
    }
}

fn lower_seq<'s>(
    stmts: &[typed::Stmt],
    result: &Option<typed::ExprId>,
    meta: &ast::Meta,
    symbols: &typed::Symbols<'s>,
    typed_nodes: &typed::ExprHeap<'s>,
    ir_nodes: &mut ir::ExprHeap<'s>,
    str_table: &mut ir::StrTable,
    lower_ctx: &mut Context,
) -> ir::ExprId {
    let stmts = stmts
        .iter()
        .map(|stmt| lower_stmt(stmt, symbols, typed_nodes, ir_nodes, str_table, lower_ctx))
        .collect();
    let result = result
        .as_ref()
        .map(|r| lower_expr(*r, symbols, typed_nodes, ir_nodes, str_table, lower_ctx));
    ir_nodes.push(ir::Expr::new(ir::ExprNode::Seq(stmts, result), meta.into()))
}

fn lower_expr<'s>(
    id: typed::ExprId,
    symbols: &typed::Symbols<'s>,
    typed_nodes: &typed::ExprHeap<'s>,
    ir_nodes: &mut ir::ExprHeap<'s>,
    str_table: &mut ir::StrTable,
    lower_ctx: &mut Context,
) -> ir::ExprId {
    let expr = &typed_nodes[id];
    use typed::ExprNode::*;
    match &expr.node {
        Apply { callee, args } => lower_apply(
            *callee,
            &args[..],
            &expr.meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        ),
        IfElse { test, then, else_ } => lower_if_else(
            *test,
            *then,
            *else_,
            &expr.meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        ),
        Case { operand, arms } => lower_case(
            *operand,
            &arms[..],
            &expr.meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        ),
        LetIn { bind, value, in_ } => lower_let_bind(
            *bind,
            *value,
            *in_,
            &expr.meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        ),
        LetPatIn { bind, value, in_ } => lower_let_pat(
            *value,
            bind,
            *in_,
            &expr.meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        ),
        MakeClosure(fid) => lower_make_closure(fid, &expr.meta, symbols, ir_nodes, &lower_ctx),
        Constructor(name) => {
            let tag = symbols.tag_for(&name);
            let expr = ir::Expr::new(ir::ExprNode::MakeTagged(tag, vec![]), (&expr.meta).into());
            ir_nodes.push(expr)
        }
        UserFunction(fid) => lower_user_function(fid, &expr.meta, ir_nodes),
        Builtin(name) => lower_builtin(&name, &expr.meta, ir_nodes),
        Tuple(elem) => lower_tuple(
            &elem[..],
            &expr.meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        ),
        Captured { id, is_self } => lower_captured(*id, *is_self, &expr.meta, ir_nodes),
        Constant(c) => lower_constant(c.clone(), &expr.meta, ir_nodes, str_table),
        Local(id) => lower_local(*id, &expr.meta, ir_nodes, lower_ctx),
        AssignLocal(local, value) => {
            let value = lower_expr(*value, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
            let r = ir::Expr::new(
                ir::ExprNode::Assign(map_local(*local, &lower_ctx), value),
                (&expr.meta).into(),
            );
            ir_nodes.push(r)
        }
        AssignCaptured(captured, value) => {
            let value = lower_expr(*value, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
            let r = ir::Expr::new(
                ir::ExprNode::Assign(map_captured(*captured, &lower_ctx), value),
                (&expr.meta).into(),
            );
            ir_nodes.push(r)
        }
        Seq(stmts, result) => lower_seq(
            &stmts,
            result,
            &expr.meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        ),
        Never => panic!("Typed AST with Never nodes can not be lowered to IR"),
    }
}

mod lower_case {
    use super::*;
    use std::collections::VecDeque;

    use ast::Pattern;

    #[derive(Clone)]
    struct Row<'s> {
        patterns: VecDeque<Option<Pattern<'s>>>,
        guard: Option<ir::ExprId>,
        binds: Vec<(ast::LocalId, ir::ExprId)>,
        result: ir::ExprId,
    }

    impl<'s> std::fmt::Debug for Row<'s> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for p in &self.patterns {
                if let Some(p) = p {
                    write!(f, "{p}, ")?;
                } else {
                    write!(f, "_, ")?;
                }
            }
            write!(f, " -> {}", self.result.0)
        }
    }

    type Operand = ir::ExprId;

    // There must be at least one row and one col
    fn col_index_with_least_binds(rows: &Vec<Row<'_>>) -> usize {
        (0..rows[0].patterns.len())
            .min_by_key(|i| {
                rows.iter()
                    .filter(|row| {
                        matches!(row.patterns[*i], Some(Pattern::Bind(_, _)))
                            || row.patterns[*i].is_none()
                    })
                    .count()
            })
            .unwrap()
    }

    fn take_col<'s>(rows: Vec<Row<'s>>, i: usize) -> Vec<(Option<Pattern<'s>>, Row<'s>)> {
        rows.into_iter()
            .map(|mut row| {
                row.patterns.swap(0, i);
                (row.patterns.pop_front().unwrap(), row)
            })
            .collect()
    }

    fn take_operand<'s>(mut header: VecDeque<Operand>, i: usize) -> (Operand, VecDeque<Operand>) {
        header.swap(0, i);
        (header.pop_front().unwrap(), header)
    }

    fn move_binds_right<'s>(
        rows: Vec<(Option<Pattern<'s>>, Row<'s>)>,
        operand: Operand,
    ) -> Vec<(Option<Pattern<'s>>, Row<'s>)> {
        rows.into_iter()
            .map(|(pat, mut row)| match pat {
                Some(Pattern::Bind(local_id, _)) => {
                    row.binds.push((local_id, operand));
                    (None, row)
                }
                _ => (pat, row),
            })
            .collect()
    }

    fn build_primitive_switch<'s>(
        operand: ir::ExprId,
        rest_operands: VecDeque<Operand>,
        mut rows: Vec<(Option<Pattern<'s>>, Row<'s>)>,
        switch_meta: &ast::Meta,
        symbols: &typed::Symbols<'s>,
        typed_nodes: &typed::ExprHeap<'s>,
        ir_nodes: &mut ir::ExprHeap<'s>,
        str_table: &mut ir::StrTable,
        lower_ctx: &mut Context,
    ) -> ir::Expr<'s> {
        let mut cases = Vec::new();

        while rows.len() != 0 {
            if let Some((first, _)) = rows
                .iter()
                .find_map(|(p, _)| p.as_ref())
                .map(|x| x.clone().unwrap_literal())
            {
                let mut branch_rows = Vec::new();
                let mut rest_rows = Vec::new();

                rows.into_iter().for_each(|(pat, r)| {
                    if let Some(pat) = pat {
                        let (pat_val, pat_meta) = pat.unwrap_literal();
                        if pat_val == first {
                            branch_rows.push(r);
                        } else {
                            rest_rows.push((Some(Pattern::Literal(pat_val, pat_meta)), r));
                        }
                    } else {
                        branch_rows.push(r.clone());
                        rest_rows.push((None, r));
                    }
                });

                rows = rest_rows;
                let first_immediate = str_table.immediate_from_constant(first);
                let switch_for_branch = lower_table(
                    rest_operands.clone(),
                    branch_rows,
                    switch_meta,
                    symbols,
                    typed_nodes,
                    ir_nodes,
                    str_table,
                    lower_ctx,
                );
                cases.push((first_immediate, switch_for_branch));
            } else {
                break;
            }
        }

        let switch_for_default = lower_table(
            rest_operands,
            rows.into_iter().map(|(_, r)| r).collect(),
            switch_meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        );

        ir::Expr::new(
            ir::ExprNode::Switch(operand, cases, switch_for_default),
            switch_meta.into(),
        )
    }

    fn build_tagged_switch<'s>(
        operand: ir::ExprId,
        rest_operands: VecDeque<Operand>,
        mut rows: Vec<(Option<Pattern<'s>>, Row<'s>)>,
        switch_meta: &ast::Meta,
        symbols: &typed::Symbols<'s>,
        typed_nodes: &typed::ExprHeap<'s>,
        ir_nodes: &mut ir::ExprHeap<'s>,
        str_table: &mut ir::StrTable,
        lower_ctx: &mut Context,
    ) -> ir::Expr<'s> {
        let mut cases = Vec::new();
        let local_for_operand = lower_ctx.new_local();

        while rows.len() != 0 {
            if let Some((first_constructor, first_args, _)) = rows
                .iter()
                .find_map(|(p, _)| p.as_ref())
                .map(|x| x.clone().unwrap_construct())
            {
                let mut branch_rows = Vec::new();
                let mut rest_rows = Vec::new();

                let first_constructor_width = first_args.len();

                rows.into_iter().for_each(|(pat, mut r)| {
                    if let Some(pat) = pat {
                        let (pat_constructor, pat_args, pat_meta) = pat.unwrap_construct();
                        if pat_constructor == first_constructor {
                            pat_args
                                .into_iter()
                                .rev()
                                .for_each(|p| r.patterns.push_front(Some(p)));
                            branch_rows.push(r);
                        } else {
                            rest_rows.push((
                                Some(Pattern::Construct(pat_constructor, pat_args, pat_meta)),
                                r,
                            ));
                        }
                    } else {
                        rest_rows.push((None, r.clone()));
                        (0..first_constructor_width).for_each(|_| r.patterns.push_front(None));
                        branch_rows.push(r);
                    }
                });

                rows = rest_rows;

                let mut rest_operands = rest_operands.clone();
                (0..first_constructor_width).rev().for_each(|i| {
                    rest_operands.push_front(ir_nodes.push(ir::Expr::new(
                        ir::ExprNode::VariantField(local_for_operand, i),
                        switch_meta.into(),
                    )))
                });

                let switch_for_branch = lower_table(
                    rest_operands,
                    branch_rows,
                    switch_meta,
                    symbols,
                    typed_nodes,
                    ir_nodes,
                    str_table,
                    lower_ctx,
                );
                let let_in_for_branch = ir::Expr::new(
                    ir::ExprNode::LetBind {
                        local: local_for_operand,
                        value: operand,
                        in_: switch_for_branch,
                    },
                    switch_meta.into(),
                );

                let first_tag = symbols.tag_for(first_constructor);
                cases.push((first_tag, ir_nodes.push(let_in_for_branch)));
            } else {
                break;
            }
        }

        let switch_for_default = lower_table(
            rest_operands,
            rows.into_iter().map(|(_, r)| r).collect(),
            switch_meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        );

        ir::Expr::new(
            ir::ExprNode::SwitchTag(operand, cases, switch_for_default),
            switch_meta.into(),
        )
    }

    fn build_tuple_deconstruct<'s>(
        operand: ir::ExprId,
        mut rest_operands: VecDeque<Operand>,
        rows: Vec<(Option<Pattern<'s>>, Row<'s>)>,
        switch_meta: &ast::Meta,
        symbols: &typed::Symbols<'s>,
        typed_nodes: &typed::ExprHeap<'s>,
        ir_nodes: &mut ir::ExprHeap<'s>,
        str_table: &mut ir::StrTable,
        lower_ctx: &mut Context,
    ) -> ir::Expr<'s> {
        let tuple_width = rows
            .iter()
            .find_map(|(p, _)| p.as_ref().map(|p| p.clone().unwrap_tuple().0.len()))
            .unwrap();
        let rows = rows
            .into_iter()
            .map(|(pat, mut r)| {
                if let Some(pat) = pat {
                    let (elems, _) = pat.unwrap_tuple();
                    elems
                        .into_iter()
                        .rev()
                        .for_each(|e| r.patterns.push_front(Some(e)));
                    r
                } else {
                    (0..tuple_width).for_each(|_| r.patterns.push_front(None));
                    r
                }
            })
            .collect();

        let local_for_operand = lower_ctx.new_local();
        (0..tuple_width).rev().for_each(|i| {
            rest_operands.push_front(ir_nodes.push(ir::Expr::new(
                ir::ExprNode::VariantField(local_for_operand, i),
                switch_meta.into(),
            )));
        });

        let switch_expr = lower_table(
            rest_operands,
            rows,
            switch_meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        );

        let let_in_for_tuple = ir::Expr::new(
            ir::ExprNode::LetBind {
                local: local_for_operand,
                value: operand,
                in_: switch_expr,
            },
            switch_meta.into(),
        );
        let_in_for_tuple
    }

    fn build_guarded<'s>(
        rows: Vec<Row<'s>>,
        switch_meta: &ast::Meta,
        ir_nodes: &mut ir::ExprHeap<'s>,
        str_table: &mut ir::StrTable,
        lower_ctx: &Context,
    ) -> ir::ExprId {
        let mut cond_pairs = Vec::new();
        let mut default = None;

        for row in &rows {
            if let Some(cond) = row.guard {
                cond_pairs.push((cond, row.result));
            } else {
                default = Some(row.result);
                break;
            }
        }

        let default = default.unwrap_or_else(|| {
            let r = ir::Expr::new(
                ir::ExprNode::panic(switch_meta, "all conditions failed", str_table),
                switch_meta.into(),
            );
            ir_nodes.push(r)
        });

        let result = if cond_pairs.len() == 0 {
            default
        } else {
            let r = ir::Expr::new(ir::ExprNode::Cond(cond_pairs, default), switch_meta.into());
            ir_nodes.push(r)
        };

        let binds = rows.iter().fold(Vec::new(), |mut accu, row| {
            accu.extend_from_slice(&row.binds);
            accu
        });

        build_binds(binds, result, switch_meta, ir_nodes, lower_ctx)
    }

    fn build_binds<'s>(
        binds: Vec<(ast::LocalId, ir::ExprId)>,
        mut result: ir::ExprId,
        switch_meta: &ast::Meta,
        ir_nodes: &mut ir::ExprHeap<'s>,
        lower_ctx: &Context,
    ) -> ir::ExprId {
        for (local, value) in binds {
            let r = ir::Expr::new(
                ir::ExprNode::LetBind {
                    local: map_local(local, lower_ctx),
                    value,
                    in_: result,
                },
                switch_meta.into(),
            );
            result = ir_nodes.push(r);
        }
        result
    }

    fn lower_table<'s>(
        header: VecDeque<Operand>,
        rows: Vec<Row<'s>>,
        switch_meta: &ast::Meta,
        symbols: &typed::Symbols<'s>,
        typed_nodes: &typed::ExprHeap<'s>,
        ir_nodes: &mut ir::ExprHeap<'s>,
        str_table: &mut ir::StrTable,
        lower_ctx: &mut Context,
    ) -> ir::ExprId {
        // no row: panic
        if rows.len() == 0 {
            let r = ir::Expr::new(
                ir::ExprNode::panic(switch_meta, "all cases failed", str_table),
                switch_meta.into(),
            );
            return ir_nodes.push(r);
        }
        // no col: use guards
        if rows[0].patterns.len() == 0 {
            return build_guarded(rows, switch_meta, ir_nodes, str_table, &lower_ctx);
        }

        let col_index_with_least_binds = col_index_with_least_binds(&rows);
        let (operand, rest_operands) = take_operand(header, col_index_with_least_binds);
        let rows = take_col(rows, col_index_with_least_binds);
        let rows = move_binds_right(rows, operand.clone());

        if let Some(first_pat) = rows.iter().find_map(|(p, _)| p.as_ref()) {
            let r = match first_pat {
                Pattern::Construct(_, _, _) => build_tagged_switch(
                    operand,
                    rest_operands,
                    rows,
                    switch_meta,
                    symbols,
                    typed_nodes,
                    ir_nodes,
                    str_table,
                    lower_ctx,
                ),
                Pattern::Literal(_, _) => build_primitive_switch(
                    operand,
                    rest_operands,
                    rows,
                    switch_meta,
                    symbols,
                    typed_nodes,
                    ir_nodes,
                    str_table,
                    lower_ctx,
                ),
                Pattern::Tuple(_, _) => build_tuple_deconstruct(
                    operand,
                    rest_operands,
                    rows,
                    switch_meta,
                    symbols,
                    typed_nodes,
                    ir_nodes,
                    str_table,
                    lower_ctx,
                ),
                _ => unreachable!(),
            };
            ir_nodes.push(r)
        } else {
            lower_table(
                rest_operands,
                rows.into_iter().map(|(_, x)| x).collect(),
                switch_meta,
                symbols,
                typed_nodes,
                ir_nodes,
                str_table,
                lower_ctx,
            )
        }
    }

    pub fn lower_case<'s>(
        operand: typed::ExprId,
        amrs: &[typed::CaseArm<'s>],
        case_meta: &ast::Meta,
        symbols: &typed::Symbols<'s>,
        typed_nodes: &typed::ExprHeap<'s>,
        ir_nodes: &mut ir::ExprHeap<'s>,
        str_table: &mut ir::StrTable,
        lower_ctx: &mut Context,
    ) -> ir::ExprId {
        let operand = lower_expr(
            operand,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        );
        let header = VecDeque::from([operand]);

        let rows = amrs
            .iter()
            .map(|arm| Row {
                patterns: VecDeque::from([Some(arm.pattern.clone())]),
                result: lower_expr(
                    arm.result,
                    symbols,
                    typed_nodes,
                    ir_nodes,
                    str_table,
                    lower_ctx,
                ),
                binds: vec![],
                guard: arm
                    .guard
                    .map(|g| lower_expr(g, symbols, typed_nodes, ir_nodes, str_table, lower_ctx)),
            })
            .collect();

        lower_table(
            header,
            rows,
            case_meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        )
    }

    pub fn lower_let_pat<'s>(
        operand: typed::ExprId,
        pattern: &ast::Pattern<'s>,
        in_: typed::ExprId,
        case_meta: &ast::Meta,
        symbols: &typed::Symbols<'s>,
        typed_nodes: &typed::ExprHeap<'s>,
        ir_nodes: &mut ir::ExprHeap<'s>,
        str_table: &mut ir::StrTable,
        lower_ctx: &mut Context,
    ) -> ir::ExprId {
        let operand = lower_expr(
            operand,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        );
        let header = VecDeque::from([operand]);

        let rows = vec![Row {
            patterns: VecDeque::from([Some(pattern.clone())]),
            guard: None,
            binds: vec![],
            result: lower_expr(in_, symbols, typed_nodes, ir_nodes, str_table, lower_ctx),
        }];

        lower_table(
            header,
            rows,
            case_meta,
            symbols,
            typed_nodes,
            ir_nodes,
            str_table,
            lower_ctx,
        )
    }
}

use lower_case::{lower_case, lower_let_pat};

fn lower_let_bind<'s>(
    local: ast::LocalId,
    value: typed::ExprId,
    in_: typed::ExprId,
    let_meta: &ast::Meta,
    symbols: &typed::Symbols<'s>,
    typed_nodes: &typed::ExprHeap<'s>,
    ir_nodes: &mut ir::ExprHeap<'s>,
    str_table: &mut ir::StrTable,
    lower_ctx: &mut Context,
) -> ir::ExprId {
    let value = lower_expr(value, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
    let in_ = lower_expr(in_, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);

    ir_nodes.push(ir::Expr::new(
        ir::ExprNode::LetBind {
            local: map_local(local, lower_ctx),
            value,
            in_,
        },
        let_meta.into(),
    ))
}

fn lower_if_else<'s>(
    test: typed::ExprId,
    then: typed::ExprId,
    else_: typed::ExprId,
    if_else_meta: &ast::Meta,
    symbols: &typed::Symbols<'s>,
    typed_nodes: &typed::ExprHeap<'s>,
    ir_nodes: &mut ir::ExprHeap<'s>,
    str_table: &mut ir::StrTable,
    lower_ctx: &mut Context,
) -> ir::ExprId {
    let test = lower_expr(test, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
    let then = lower_expr(then, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
    let else_ = lower_expr(else_, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);

    ir_nodes.push(ir::Expr::new(
        ir::ExprNode::IfElse { test, then, else_ },
        if_else_meta.into(),
    ))
}

fn lower_tuple<'s>(
    elems: &[typed::ExprId],
    tuple_meta: &ast::Meta,
    symbols: &typed::Symbols<'s>,
    typed_nodes: &typed::ExprHeap<'s>,
    ir_nodes: &mut ir::ExprHeap<'s>,
    str_table: &mut ir::StrTable,
    lower_ctx: &mut Context,
) -> ir::ExprId {
    let elems: Vec<_> = elems
        .iter()
        .map(|id| lower_expr(*id, symbols, typed_nodes, ir_nodes, str_table, lower_ctx))
        .collect();
    ir_nodes.push(ir::Expr::new(
        ir::ExprNode::MakeTuple(elems),
        tuple_meta.into(),
    ))
}

fn lower_apply<'s>(
    callee: typed::ExprId,
    args: &[typed::ExprId],
    call_meta: &ast::Meta,
    symbols: &typed::Symbols<'s>,
    typed_nodes: &typed::ExprHeap<'s>,
    ir_nodes: &mut ir::ExprHeap<'s>,
    str_table: &mut ir::StrTable,
    lower_ctx: &mut Context,
) -> ir::ExprId {
    if matches!(&typed_nodes[callee].node, typed::ExprNode::Builtin("panic")) {
        match &typed_nodes[args[0]].node {
            typed::ExprNode::Constant(c) => match c {
                ast::Constant::Str(msg) => {
                    return lower_panic(*&msg, call_meta, ir_nodes, str_table);
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
    }

    let args: Vec<_> = args
        .iter()
        .map(|arg| lower_expr(*arg, symbols, typed_nodes, ir_nodes, str_table, lower_ctx))
        .collect();

    let expr = match &typed_nodes[callee].node {
        typed::ExprNode::Constructor(constructor) => {
            let tag = symbols.tag_for(*&constructor);
            ir::Expr::new(ir::ExprNode::MakeTagged(tag, args), call_meta.into())
        }
        _ => {
            let callee_impure = typed_nodes[callee].type_.node.impure();
            let callee = lower_expr(callee, symbols, typed_nodes, ir_nodes, str_table, lower_ctx);
            ir::Expr::new(
                ir::ExprNode::Apply {
                    callee,
                    args,
                    callee_impure,
                },
                call_meta.into(),
            )
        }
    };

    ir_nodes.push(expr)
}

fn lower_panic<'s>(
    msg: &str,
    panic_meta: &ast::Meta,
    ir_nodes: &mut ir::ExprHeap<'s>,
    str_table: &mut ir::StrTable,
) -> ir::ExprId {
    let expr = ir::Expr::new(
        ir::ExprNode::panic_string(panic_meta, msg.to_string(), str_table),
        panic_meta.into(),
    );
    ir_nodes.push(expr)
}

fn lower_builtin<'s>(
    name: &'s str,
    builtin_meta: &ast::Meta,
    ir_nodes: &mut ir::ExprHeap<'s>,
) -> ir::ExprId {
    ir_nodes.push(ir::Expr::new(
        ir::ExprNode::Builtin(name),
        builtin_meta.into(),
    ))
}

fn lower_user_function<'s>(
    name: &'s str,
    global_meta: &ast::Meta,
    ir_nodes: &mut ir::ExprHeap,
) -> ir::ExprId {
    ir_nodes.push(ir::Expr::new(
        ir::ExprNode::UserFunction(name.to_string()),
        global_meta.into(),
    ))
}

fn lower_constant<'s>(
    c: ast::Constant<'s>,
    constant_meta: &ast::Meta,
    ir_nodes: &mut ir::ExprHeap,
    str_table: &mut ir::StrTable,
) -> ir::ExprId {
    let immediate = str_table.immediate_from_constant(c);
    ir_nodes.push(ir::Expr::new(
        ir::ExprNode::Immediate(immediate),
        constant_meta.into(),
    ))
}

/// Captured values are passed in as the first few locals, and curried functions store already-passed-in arguments
/// right after captured values, then when all arguments are passed, we simply push both captured values and arguments
/// to stack
fn lower_captured<'s>(
    id: ast::CapturedId,
    is_self: bool,
    captured_meta: &ast::Meta,
    ir_nodes: &mut ir::ExprHeap,
) -> ir::ExprId {
    let node = if is_self {
        ir::ExprNode::ThisClosure(id.0.into())
    } else {
        ir::ExprNode::Local(id.0.into())
    };
    ir_nodes.push(ir::Expr::new(node, captured_meta.into()))
}

fn map_local(id: ast::LocalId, lower_ctx: &Context) -> ir::LocalId {
    ir::LocalId::from(id.0 + lower_ctx.captured_cnt)
}

fn map_captured(id: ast::CapturedId, _lower_ctx: &Context) -> ir::LocalId {
    ir::LocalId::from(id.0)
}

fn lower_local<'s>(
    id: ast::LocalId,
    local_meta: &ast::Meta,
    ir_nodes: &mut ir::ExprHeap,
    lower_ctx: &mut Context,
) -> ir::ExprId {
    ir_nodes.push(ir::Expr::new(
        ir::ExprNode::Local(map_local(id, &lower_ctx)),
        local_meta.into(),
    ))
}

fn lower_make_closure<'s>(
    fid: &ast::FunctionId,
    closure_meta: &ast::Meta,
    symbols: &typed::Symbols,
    ir_nodes: &mut ir::ExprHeap,
    lower_ctx: &Context,
) -> ir::ExprId {
    let f = symbols.function(fid);
    ir_nodes.push(ir::Expr::new(
        ir::ExprNode::MakeClosure(
            fid.clone(),
            f.captures
                .iter()
                .copied()
                .map(|i| map_local(i, lower_ctx))
                .collect(),
        ),
        closure_meta.into(),
    ))
}
