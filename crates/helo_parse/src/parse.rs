use std::collections::HashMap;

use crate::ast;
use crate::errors;

use nom::branch as nbr;
use nom::bytes::complete as nbyte;
use nom::character::complete as nchar;
use nom::combinator as ncomb;
use nom::multi as nmulti;
use nom::sequence as nseq;
use nom::Finish;

type OpPriority = u32;

pub struct Precedence(OpPriority, OpPriority);

impl Precedence {
    pub fn left(&self) -> OpPriority {
        self.0
    }
    pub fn right(&self) -> OpPriority {
        self.1
    }
}

pub struct PrecedenceTable<'s>(HashMap<&'s str, Precedence>);

impl<'s> PrecedenceTable<'s> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, k: &'s str, v: Precedence) -> Option<Precedence> {
        self.0.insert(k, v)
    }

    pub fn get(&self, k: &str) -> Option<&Precedence> {
        self.0.get(k)
    }
}

type ResolutionEntry<'s> = (&'s str, ast::LocalId);

#[derive(Default)]
struct FunctionResolutionEnv<'s> {
    locals: Vec<ResolutionEntry<'s>>,
    scope_cnt: Vec<usize>,
    id_cnt: ast::LocalId,
}

impl<'s> FunctionResolutionEnv<'s> {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            scope_cnt: vec![0],
            id_cnt: 0.into(),
        }
    }
    fn local_cnt(&self) -> usize {
        self.id_cnt.into()
    }
}

enum ResolutionEnv<'s> {
    Normal(FunctionResolutionEnv<'s>),
    InClosure {
        parent: FunctionResolutionEnv<'s>,
        current: FunctionResolutionEnv<'s>,
        captures: Vec<ast::LocalId>,
        caputures_meta: Vec<ast::Meta>,
        this_name: &'s str,
    },
}

impl<'s> ResolutionEnv<'s> {
    fn current(&mut self) -> &mut FunctionResolutionEnv<'s> {
        match self {
            ResolutionEnv::Normal(current) => current,
            ResolutionEnv::InClosure { current, .. } => current,
        }
    }
    fn pop(&mut self, cnt: usize) {
        let new_len = self.current().locals.len() - cnt;
        self.current()
            .locals
            .resize_with(new_len, || unreachable!());
    }
    fn pop_scope(&mut self) {
        let pop_cnt = self.current().scope_cnt.pop().unwrap();
        self.pop(pop_cnt);
    }
}

struct Context<'s, 'a> {
    file_name: std::sync::Arc<String>,
    src: std::sync::Arc<String>,
    ast_nodes: &'a mut ast::ExprHeap<'s>,
    e: &'a mut errors::ManyError,
    resolution_env: ResolutionEnv<'s>,
    generic_params: Vec<&'s str>,
    precedence_table: &'a mut PrecedenceTable<'s>,
    allow_wildcard_in_type: bool,
    pub symbols: &'a mut ast::Symbols<'s>,
}

impl<'s, 'a> Context<'s, 'a> {
    pub fn new(
        file_name: std::sync::Arc<String>,
        src: std::sync::Arc<String>,
        ast_nodes: &'a mut ast::ExprHeap<'s>,
        e: &'a mut errors::ManyError,
        ptable: &'a mut PrecedenceTable<'s>,
        symbols: &'a mut ast::Symbols<'s>,
    ) -> Self {
        Self {
            file_name,
            src,
            ast_nodes,
            e,
            resolution_env: ResolutionEnv::Normal(FunctionResolutionEnv::new()),
            generic_params: Vec::new(),
            precedence_table: ptable,
            allow_wildcard_in_type: false,
            symbols,
        }
    }

    /// Get left priority of infix operator `id`. Record an error and return 0 if `id` is not registered as an infix operator
    pub fn priority_left(&mut self, id: &str, id_meta: &ast::Meta) -> OpPriority {
        self.precedence_table.get(id).map(|p| p.left()).map_or_else(
            || {
                self.e
                    .push(errors::UndeclaredInfixOperator::new(id_meta, id));
                0
            },
            |x| x,
        )
    }
    pub fn priority_right(&mut self, id: &str, id_meta: &ast::Meta) -> OpPriority {
        self.precedence_table
            .get(id)
            .map(|p| p.right())
            .map_or_else(
                || {
                    self.e
                        .push(errors::UndeclaredInfixOperator::new(id_meta, id));
                    0
                },
                |x| x,
            )
    }

    pub fn meta(&self, begin: &str, end: &str) -> ast::Meta {
        let span = (self.src.len() - begin.len(), begin.len() - end.len());
        ast::Meta {
            span,
            file_name: self.file_name.clone(),
            src: self.src.clone(),
        }
    }

    /// Resolve identifier as local variable
    pub fn resolve(&mut self, name: &str, meta: ast::Meta) -> Option<ast::Expr<'s>> {
        match self {
            Self {
                resolution_env: ResolutionEnv::Normal(f_res),
                ..
            } => f_res
                .locals
                .iter()
                .rfind(|(n, _)| *n == name)
                .map(|(_, id)| ast::Expr::new_untyped(ast::ExprNode::Local(*id), meta)),

            // See documentation on [`ast::Closure_`] for how locals are laid out
            Self {
                resolution_env:
                    ResolutionEnv::InClosure {
                        parent,
                        current,
                        captures,
                        caputures_meta,
                        this_name,
                    },
                ..
            } => {
                current
                    .locals
                    .iter()
                    .rfind(|(n, _)| *n == name)
                    .map_or_else(
                        // Identifier not found as local, look in parent namespace, that is the function body surrounding the closure
                        || {
                            parent.locals.iter().rfind(|(n, _)| *n == name).map_or(
                                None,
                                |(_, id)| {
                                    // Found in parent namespace
                                    if let Some(i) = captures.iter().position(|i| *i == *id) {
                                        Some(ast::Expr::new_untyped(
                                            ast::ExprNode::Captured(i.into(), *this_name == name),
                                            meta.clone(),
                                        ))
                                    } else {
                                        captures.push(*id);
                                        caputures_meta.push(meta.clone());
                                        Some(ast::Expr::new_untyped(
                                            ast::ExprNode::Captured(
                                                (captures.len() - 1).into(),
                                                *this_name == name,
                                            ),
                                            meta.clone(),
                                        ))
                                    }
                                },
                            )
                        },
                        // Local
                        |(_, id)| {
                            Some(ast::Expr::new_untyped(
                                ast::ExprNode::Local(*id),
                                meta.clone(),
                            ))
                        },
                    )
            }
        }
    }

    pub fn push_expr(&mut self, node: ast::Expr<'s>) -> ast::ExprId {
        self.ast_nodes.push(node)
    }

    pub fn define_local(&mut self, name: &'s str) -> ast::LocalId {
        let id = self.resolution_env.current().id_cnt;
        self.resolution_env.current().locals.push((name, id));
        *self.resolution_env.current().scope_cnt.last_mut().unwrap() += 1;
        self.resolution_env.current().id_cnt = id + 1;
        id
    }

    pub fn with_function_scope<R>(&mut self, f: impl Fn(&mut Context<'s, '_>) -> R) -> (usize, R) {
        self.resolution_env = ResolutionEnv::Normal(FunctionResolutionEnv::new());
        let r = f(self);
        (self.resolution_env.current().local_cnt(), r)
    }

    /// All variables defined in `f` will be popped upon exit
    pub fn with_scope<R>(&mut self, f: impl FnOnce(&mut Context<'s, '_>) -> R) -> R {
        self.resolution_env.current().scope_cnt.push(0);
        let r = f(self);

        self.resolution_env.pop_scope();
        r
    }

    /// Set current status to "in-closure", that is , identifiers are resolved further in surrunding namespace
    /// if they are not found in local namespace.
    /// Return (captures, number of locals, result of `f`)
    pub fn with_closure_scope<R>(
        &mut self,
        this_name: &'s str,
        f: impl Fn(&mut Context<'s, '_>) -> R,
    ) -> (Vec<ast::LocalId>, Vec<ast::Meta>, usize, R) {
        let parent = match &mut self.resolution_env {
            ResolutionEnv::Normal(p) => std::mem::take(p),
            _ => unreachable!(),
        };
        self.resolution_env = ResolutionEnv::InClosure {
            parent,
            current: FunctionResolutionEnv::new(),
            captures: Vec::new(),
            caputures_meta: Vec::new(),
            this_name,
        };

        let r = f(self);

        let (parent, captures, captures_meta, locals_cnt) = match &mut self.resolution_env {
            ResolutionEnv::InClosure {
                parent,
                captures,
                current,
                caputures_meta,
                ..
            } => (
                std::mem::take(parent),
                std::mem::take(captures),
                std::mem::take(caputures_meta),
                current.local_cnt(),
            ),
            _ => unreachable!(),
        };
        self.resolution_env = ResolutionEnv::Normal(parent);
        (captures, captures_meta, locals_cnt, r)
    }

    fn with_allow_wildcard_in_type<R>(&mut self, f: impl Fn(&mut Context<'s, '_>) -> R) -> R {
        self.allow_wildcard_in_type = true;
        let r = f(self);
        self.allow_wildcard_in_type = false;
        r
    }

    pub fn set_generic_params(&mut self, v: Vec<&'s str>) {
        self.generic_params = v;
    }
    pub fn resolve_generic_param(&self, name: &str) -> Option<ast::TypeVarId> {
        self.generic_params
            .iter()
            .position(|p| *p == name)
            .map(|i| i.into())
    }

    pub fn push_expr_many(
        &mut self,
        exprs: impl Iterator<Item = ast::Expr<'s>>,
    ) -> Vec<ast::ExprId> {
        self.ast_nodes.push_many(exprs)
    }

    pub fn generic_param_cnt(&self) -> usize {
        self.generic_params.len()
    }

    pub fn push_error(&mut self, e: impl miette::Diagnostic + 'static + Send + Sync) {
        self.e.push(e)
    }
}

macro_rules! alt {
    ($s:expr, $ctx:expr, $parser_first:ident $(,$parser:ident)* ) => {{
            if let (s1, Some(p)) = ncomb::opt(|s| $parser_first(s, $ctx))($s)? {
                return Ok((s1, p));
            }
         $(
            if let (s1, Some(p)) = ncomb::opt(|s| $parser(s, $ctx))($s)? {
                return Ok((s1, p));
            }
        )*
        return Err(nom::Err::Error(nom::error::Error::new(
            $s,
            nom::error::ErrorKind::Alt,
        )));
    }  };
}

type NomE<'s> = nom::error::Error<&'s str>;
type PResult<'s, T> = nom::IResult<&'s str, T, NomE<'s>>;
type MResult<'s, U> = PResult<'s, (U, ast::Meta)>;
type EResult<'s> = PResult<'s, ast::Expr<'s>>;

fn empty(s: &str) -> PResult<'_, ()> {
    let (s, _) = nchar::multispace0(s)?;
    Ok((s, ()))
}
fn trailing_space<'a, O, F>(mut f: F) -> impl FnMut(&'a str) -> PResult<'a, O>
where
    F: nom::Parser<&'a str, O, NomE<'a>>,
{
    move |s| {
        let (s1, r) = f.parse(s)?;
        let (s1, _) = empty(s1)?;
        Ok((s1, r))
    }
}

// const symbolic_keywords: [&'static str; 3] = ["|", ")", ","];
const ALPHABETICAL_KEYWORDS: [&'static str; 8] =
    ["then", "else", "of", "case", "let", "fn", "in", "data"];

/// An identifier that is made purely of a set of symbols. NOTE that the `_a` suffix means trailing spaces
/// are not consumed
fn symbolic_identifier_str_a<'s>(
    s: &'s str,
    ctx: &Context<'s, '_>,
) -> PResult<'s, (&'s str, ast::Meta)> {
    let (s1, id) = nbyte::is_a("~!@#$%^&*<>=+-./")(s)?;
    Ok((s1, (id, ctx.meta(s, s1))))
}

/// An identifier that is not started with a number, and doesn't contain any symbol
fn alphabetic_identifier_str_a<'s>(
    s: &'s str,
    ctx: &Context<'s, '_>,
) -> PResult<'s, (&'s str, ast::Meta)> {
    let (s1, _) = nchar::none_of("0123456789~/<>,.:\"'[]{}|\\+=-()*&^%$#@!` \r\n")(s)?;
    let (s2, left) = ncomb::opt(nbyte::is_not("~/<>,.:\"'[]{}|\\+=-()*&^%$#@!` \r\n"))(s1)?;

    let (s2, (id, meta)) = if let Some(left) = left {
        (s2, (&s[0..left.len() + 1], ctx.meta(s, s2)))
    } else {
        (s2, (&s[0..1], ctx.meta(s, s2)))
    };

    if ALPHABETICAL_KEYWORDS.contains(&id) {
        Err(nom::Err::Error(nom::error::Error::new(
            s,
            nom::error::ErrorKind::Alpha,
        )))
    } else {
        Ok((s2, (id, meta)))
    }
}

/// An `alphabetic_identifier` that is surrounded by "`", indicating that it's used as an infix operator
fn infix_alphabetic_identifier_str_a<'s>(
    s: &'s str,
    ctx: &Context<'s, '_>,
) -> PResult<'s, (&'s str, ast::Meta)> {
    let (s1, _) = nbyte::tag("`")(s)?;
    let (s2, (id_mid, _)) = alphabetic_identifier_str_a(s1, ctx)?;
    let (s3, _) = nbyte::tag("`")(s2)?;
    Ok((s3, (&s[0..id_mid.len() + 2], ctx.meta(s, s3))))
}

/// An identifier that is used as an infix operator
fn infix_identifier_str_a<'s>(
    s: &'s str,
    ctx: &Context<'s, '_>,
) -> PResult<'s, (&'s str, ast::Meta)> {
    alt!(
        s,
        ctx,
        symbolic_identifier_str_a,
        infix_alphabetic_identifier_str_a
    );
}

fn infix_identifier_str<'s>(
    s: &'s str,
    ctx: &Context<'s, '_>,
) -> PResult<'s, (&'s str, ast::Meta)> {
    trailing_space(|s| infix_identifier_str_a(s, ctx))(s)
}

/// Like `alphabetic_identifier_str_a`, but removes trailing space
fn alphabetic_identifier_str<'s>(
    s: &'s str,
    ctx: &Context<'s, '_>,
) -> PResult<'s, (&'s str, ast::Meta)> {
    let (s1, r) = alphabetic_identifier_str_a(s, ctx)?;
    let (s2, _) = empty(s1)?;
    Ok((s2, r))
}

/// A declaration of an infix operator
fn infix_decl<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ()> {
    let (s1, _) = trailing_space(nbyte::tag("infix"))(s)?;

    let (s2, (id, _)) = infix_identifier_str(s1, ctx)?;

    let (s3, p_left) = trailing_space(nchar::u32)(s2)?;
    let (s3, p_right) = trailing_space(nchar::u32)(s3)?;

    ctx.precedence_table.insert(id, Precedence(p_left, p_right));
    Ok((s3, ()))
}

/// A number literal, which can either be float or int. The `_expr` suffix indicates that it parses an expression.
fn num_literal_expr<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    let (s1, (c, m)) = trailing_space(|s| num_literal_a(s, ctx))(s)?;
    Ok((s1, ast::Expr::new_untyped(ast::ExprNode::Constant(c), m)))
}

fn num_literal_a<'s>(s: &'s str, ctx: &Context<'s, '_>) -> MResult<'s, ast::Constant<'s>> {
    let (s1, d1) = nchar::digit1(s)?;
    let (s2, d2) = ncomb::opt(nseq::preceded(nbyte::tag("."), nchar::digit1))(s1)?;
    if let Some(d2) = d2 {
        Ok((
            s2,
            (
                ast::Constant::Float(s[0..d1.len() + 1 + d2.len()].parse().unwrap()),
                ctx.meta(s, s2),
            ),
        ))
    } else {
        Ok((
            s2,
            (ast::Constant::Int(d1.parse().unwrap()), ctx.meta(s, s2)),
        ))
    }
}

/// Like `num_literal_expr`
fn string_literal_expr<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    let (s1, (c, m)) = trailing_space(|s| string_literal_a(s, ctx))(s)?;
    Ok((s1, ast::Expr::new_untyped(ast::ExprNode::Constant(c), m)))
}

fn string_literal_a<'s>(s: &'s str, ctx: &Context<'s, '_>) -> MResult<'s, ast::Constant<'s>> {
    let (s1, _) = nbyte::tag("\"")(s)?;
    let (s2, string) = nbyte::take_until("\"")(s1)?;
    let (s3, _) = nbyte::tag("\"")(s2)?;
    Ok((s3, (ast::Constant::Str(string), ctx.meta(s, s3))))
}

fn if_expr<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    let (s2, test) = expression(s, ctx)?;
    let test = ctx.push_expr(test);

    let (s3, _) = trailing_space(nbyte::tag("then"))(s2)?;
    let (s4, then) = expression(s3, ctx)?;
    let then = ctx.push_expr(then);

    let (s5, _) = trailing_space(nbyte::tag("else"))(s4)?;
    let (s6, else_) = expression(s5, ctx)?;
    let else_ = ctx.push_expr(else_);

    Ok((
        s6,
        ast::Expr::new_untyped(ast::ExprNode::IfElse { test, then, else_ }, ctx.meta(s, s6)),
    ))
}

/// A constructor pattern, like `Some 1`
fn pattern_construct<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Pattern<'s>> {
    let (s1, (constructor, template_meta)) = alphabetic_identifier_str(s, ctx)?;

    if !constructor.chars().next().unwrap().is_uppercase() {
        let local_id = ctx.define_local(constructor);
        return Ok((s1, ast::Pattern::Bind(local_id, template_meta)));
    }

    let (s2, args) =
        nmulti::separated_list0(trailing_space(nbyte::tag(",")), |s| pattern(s, ctx))(s1)?;
    Ok((
        s2,
        ast::Pattern::Construct(constructor, args, ctx.meta(s, s2)),
    ))
}

/// A literal pattern
fn pattern_literal<'s>(s: &'s str, ctx: &Context<'s, '_>) -> PResult<'s, ast::Pattern<'s>> {
    let (s1, (value, meta)) =
        nbr::alt((|s| string_literal_a(s, ctx), |s| num_literal_a(s, ctx)))(s)?;
    let (s1, _) = empty(s1)?;
    Ok((s1, ast::Pattern::Literal(value, meta)))
}

/// A bind pattern. NOTE that the bound identifier can only be alphabetic
fn pattern_bind<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Pattern<'s>> {
    let (s1, (id, meta)) = alphabetic_identifier_str(s, ctx)?;
    let local_id = ctx.define_local(id);
    Ok((s1, ast::Pattern::Bind(local_id, meta)))
}

fn parenthesed_pattern<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Pattern<'s>> {
    let (s1, _) = trailing_space(nbyte::tag("("))(s)?;
    let (s1, mut pats) =
        nmulti::separated_list1(trailing_space(nbyte::tag(",")), |s| pattern(s, ctx))(s1)?;
    let (s2, _) = nbyte::tag(")")(s1)?;
    let (s3, _) = empty(s2)?;

    if pats.len() == 1 {
        Ok((s3, pats.pop().unwrap()))
    } else {
        Ok((s3, ast::Pattern::Tuple(pats, ctx.meta(s, s2))))
    }
}

/// A pattern
fn pattern<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Pattern<'s>> {
    alt!(
        s,
        ctx,
        parenthesed_pattern,
        pattern_construct,
        pattern_literal,
        pattern_bind
    );
}

fn case_arm<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::CaseArm<'s>> {
    ctx.with_scope(|ctx| {
        let (s1, pat) = pattern(s, ctx)?;

        let (s2, guard) = if let (s2, Some(_)) = ncomb::opt(trailing_space(nbyte::tag("if")))(s1)? {
            let (s3, guard) = expression(s2, ctx)?;
            (s3, Some(guard))
        } else {
            (s1, None)
        };

        let (s2, _) = trailing_space(nbyte::tag("->"))(s2)?;
        let (s3, result) = expression(s2, ctx)?;
        Ok((
            s3,
            ast::CaseArm {
                pattern: pat,
                guard: guard.map(|x| ctx.push_expr(x)),
                result: ctx.push_expr(result),
            },
        ))
    })
}

fn case_expr<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    let (s2, operand) = expression(s, ctx)?;
    let operand = ctx.push_expr(operand);

    let (s3, _) = trailing_space(nbyte::tag("of"))(s2)?;

    let (s4, arms) =
        nmulti::separated_list1(trailing_space(nbyte::tag("|")), |s| case_arm(s, ctx))(s3)?;
    Ok((
        s4,
        ast::Expr::new_untyped(ast::ExprNode::Case { operand, arms }, ctx.meta(s, s4)),
    ))
}

fn let_in_bind_expr<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    let (s1, value) = expression(s, ctx)?;
    let value = ctx.push_expr(value);

    let (s1, _) = trailing_space(nbyte::tag("of"))(s1)?;

    ctx.with_scope(|ctx| {
        let (s2, pattern) = pattern(s1, ctx)?;

        let (s3, _) = trailing_space(nbyte::tag("in"))(s2)?;
        let (s4, in_) = expression(s3, ctx)?;
        Ok((
            s4,
            ast::Expr::new_untyped(
                ast::ExprNode::LetPatIn {
                    bind: pattern,
                    value,
                    in_: ctx.push_expr(in_),
                },
                ctx.meta(s, s4),
            ),
        ))
    })
}

fn function_params<'s>(
    s: &'s str,
    ctx: &mut Context<'s, '_>,
) -> PResult<'s, (Vec<&'s str>, Vec<ast::Meta>)> {
    let (s1, r) = nmulti::separated_list0(trailing_space(nbyte::tag(",")), |s| {
        alphabetic_identifier_str(s, ctx)
    })(s)?;
    let (params, meta) = r.into_iter().unzip();
    Ok((s1, (params, meta)))
}

fn let_in_closure_expr<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    let (s1, _) = trailing_space(nbyte::tag("fn"))(s)?;

    let (s1, (id, _)) = alphabetic_identifier_str(s1, ctx)?;

    ctx.with_scope(|ctx| {
        let local_id = ctx.define_local(id);

        let (s2, params) = nmulti::separated_list0(trailing_space(nbyte::tag(",")), |s| {
            alphabetic_identifier_str(s, ctx)
        })(s1)?;
        let (params, params_meta): (Vec<_>, Vec<_>) = params.into_iter().unzip();

        let (s3, _) = trailing_space(nbyte::tag("="))(s2)?;

        let (captures, captures_meta, local_cnt, r) = ctx.with_closure_scope(id, |ctx| {
            params.iter().for_each(|p| {
                ctx.define_local(p);
            });
            expression(s3, ctx)
        });
        let (s4, body) = r?;

        let f = ast::Function {
            type_: None,
            var_cnt: 0,
            local_cnt,
            arity: params.len(),
            body: ctx.push_expr(body),
            meta: ctx.meta(s, s4),
            param_metas: params_meta,
            captures,
            captures_meta,
        };
        let closure_meta = ctx.meta(s, s4);
        let closure_id = closure_meta.closure_id();
        ctx.symbols.add_function(closure_id.clone(), f);

        let closure_expr = ctx.push_expr(ast::Expr::new_untyped(
            ast::ExprNode::MakeClosure(closure_id),
            closure_meta,
        ));

        let (s5, _) = trailing_space(nbyte::tag("in"))(s4)?;
        let (s6, in_) = expression(s5, ctx)?;
        Ok((
            s6,
            ast::Expr::new_untyped(
                ast::ExprNode::LetIn {
                    bind: local_id,
                    value: closure_expr,
                    in_: ctx.push_expr(in_),
                },
                ctx.meta(s, s6),
            ),
        ))
    })
}

fn let_in_expr<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    alt!(s, ctx, let_in_closure_expr, let_in_bind_expr)
}

fn generic_param_a<'s>(s: &'s str, ctx: &Context<'s, '_>) -> MResult<'s, &'s str> {
    let (s1, _) = nbyte::tag("'")(s)?;
    let (s2, name) = nchar::alphanumeric1(s1)?;
    Ok((s2, (name, ctx.meta(s, s2))))
}

fn generic_params_decl<'s>(
    s: &'s str,
    ctx: &Context<'s, '_>,
) -> PResult<'s, (Vec<&'s str>, Vec<ast::Meta>)> {
    let (s1, generics) = nmulti::separated_list0(
        trailing_space(nbyte::tag(",")),
        trailing_space(|s| generic_param_a(s, ctx)),
    )(s)?;
    Ok((s1, generics.into_iter().unzip()))
}

fn type_generic<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Type<'s>> {
    let (s1, (template, template_meta)) = alphabetic_identifier_str(s, ctx)?;

    if template == "_" && ctx.allow_wildcard_in_type {
        return Ok((
            s1,
            ast::Type {
                node: ast::TypeNode::WildCard,
                meta: ctx.meta(s, s1),
            },
        ));
    }

    let (s4, args, meta) = if let (s2, Some(_)) = ncomb::opt(trailing_space(nbyte::tag("[")))(s1)? {
        let (s3, args) =
            nmulti::separated_list1(trailing_space(nbyte::tag(",")), |s| type_(s, ctx))(s2)?;
        let (s4, _) = nbyte::tag("]")(s3)?;
        let (s5, _) = empty(s4)?;
        (s5, args, ctx.meta(s, s4))
    } else {
        (s1, vec![], template_meta)
    };

    Ok((
        s4,
        ast::Type {
            node: ast::TypeNode::Generic(template, args),
            meta,
        },
    ))
}

fn type_callable<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Type<'s>> {
    let (s1, callable) = type_callable_type(s, ctx)?;

    Ok((
        s1,
        ast::Type {
            node: ast::TypeNode::Callable(callable),
            meta: ctx.meta(s, s1),
        },
    ))
}

fn type_callable_type<'s>(
    s: &'s str,
    ctx: &mut Context<'s, '_>,
) -> PResult<'s, ast::CallableType<'s>> {
    let (s1, _) = trailing_space(nbyte::tag("["))(s)?;
    let (s2, params) =
        nmulti::separated_list1(trailing_space(nbyte::tag(",")), |s| type_(s, ctx))(s1)?;
    let (s3, _) = trailing_space(nbyte::tag("]"))(s2)?;

    let (s3, _) = trailing_space(nbyte::tag("->"))(s3)?;
    let (s4, ret) = type_(s3, ctx)?;

    Ok((
        s4,
        ast::CallableType {
            params,
            ret: Box::new(ret),
        },
    ))
}

fn type_primitive<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Type<'s>> {
    let (s1, pri) = nbr::alt((
        ncomb::map(nbyte::tag("Int"), |_| ast::PrimitiveType::Int),
        ncomb::map(nbyte::tag("Float"), |_| ast::PrimitiveType::Float),
        ncomb::map(nbyte::tag("Str"), |_| ast::PrimitiveType::Str),
        ncomb::map(nbyte::tag("Bool"), |_| ast::PrimitiveType::Bool),
    ))(s)?;
    let (s2, _) = empty(s1)?;

    Ok((
        s2,
        ast::Type {
            node: ast::TypeNode::Primitive(pri),
            meta: ctx.meta(s, s1),
        },
    ))
}

fn type_unit<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Type<'s>> {
    let (s1, _) = nbyte::tag("()")(s)?;
    let (s2, _) = empty(s1)?;
    Ok((
        s2,
        ast::Type {
            node: ast::TypeNode::Unit,
            meta: ctx.meta(s, s1),
        },
    ))
}

fn type_var<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Type<'s>> {
    let (s1, (gp, meta)) = trailing_space(|s| generic_param_a(s, ctx))(s)?;
    let type_node = ctx
        .resolve_generic_param(gp)
        .map_or(ast::TypeNode::Never, |var_id| ast::TypeNode::Var(var_id));
    Ok((
        s1,
        ast::Type {
            node: type_node,
            meta,
        },
    ))
}

fn type_tuple<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Type<'s>> {
    let (s1, _) = trailing_space(nbyte::tag("("))(s)?;
    let (s2, args) =
        nmulti::separated_list1(trailing_space(nbyte::tag(",")), |s| type_(s, ctx))(s1)?;
    let (s3, _) = nbyte::tag(")")(s2)?;
    let (s4, _) = empty(s3)?;

    Ok((
        s4,
        ast::Type {
            node: ast::TypeNode::Tuple(args),
            meta: ctx.meta(s, s3),
        },
    ))
}

fn type_<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ast::Type<'s>> {
    alt!(
        s,
        ctx,
        type_var,
        type_tuple,
        type_primitive,
        type_generic,
        type_callable,
        type_unit
    );
}

fn constructor<'s>(
    s: &'s str,
    ctx: &mut Context<'s, '_>,
    data_name: &'s str,
) -> PResult<'s, ast::Constructor<'s>> {
    let (s1, (name, c_meta)) = alphabetic_identifier_str(s, ctx)?;

    if !name.chars().next().unwrap().is_uppercase() {
        ctx.push_error(errors::ConstructorNameNotUppercase::new(&c_meta));
    }

    let (s2, params) =
        nmulti::separated_list0(trailing_space(nbyte::tag(",")), |s| type_(s, ctx))(s1)?;
    Ok((
        s2,
        ast::Constructor {
            name,
            params,
            belongs_to: data_name,
            meta: ctx.meta(s, s2),
        },
    ))
}

fn data<'s>(
    s: &'s str,
    ctx: &mut Context<'s, '_>,
) -> PResult<'s, (&'s str, ast::Data<'s>, Vec<ast::Constructor<'s>>)> {
    let (s1, _) = trailing_space(nbyte::tag("data"))(s)?;
    let (s2, (data_name, _)) = alphabetic_identifier_str(s1, ctx)?;
    let (s2, _) = trailing_space(nbyte::tag("["))(s2)?;

    let (s3, (generic_params, generic_metas)) = generic_params_decl(s2, ctx)?;
    let kind_arity = generic_params.len();
    ctx.set_generic_params(generic_params);

    let (s3, _) = trailing_space(nbyte::tag("]"))(s3)?;

    let (s4, _) = trailing_space(nbyte::tag("="))(s3)?;

    let (s5, constructors) = nmulti::separated_list1(trailing_space(nbyte::tag("|")), |s| {
        constructor(s, ctx, data_name)
    })(s4)?;

    let data = ast::Data {
        name: data_name,
        kind_arity,
        constructors: constructors.iter().map(|c| c.name).collect(),
        meta: ctx.meta(s, s5),
        generic_metas,
    };

    Ok((s5, (data_name, data, constructors)))
}

fn resolve_identifier<'s>(
    id: &'s str,
    meta: ast::Meta,
    ctx: &mut Context<'s, '_>,
) -> ast::Expr<'s> {
    if let Some(result) = ctx.resolve(id, meta.clone()) {
        result
    } else {
        ast::Expr::new_untyped(ast::ExprNode::Global(id), meta)
    }
}

fn function_name_identifier_a<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> MResult<'s, &'s str> {
    alt!(
        s,
        ctx,
        symbolic_identifier_str_a,
        alphabetic_identifier_str_a
    );
}

fn function_name_identifier<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> MResult<'s, &'s str> {
    trailing_space(|s| function_name_identifier_a(s, ctx))(s)
}

fn identifier_expr<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    let (s1, (id, meta)) = trailing_space(|s| function_name_identifier_a(s, ctx))(s)?;
    Ok((s1, resolve_identifier(id, meta, ctx)))
}

fn expression_item<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    alt!(
        s,
        ctx,
        identifier_expr,
        num_literal_expr,
        string_literal_expr
    );
}

fn expression_parenthesed<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    let (s1, mut exprs) =
        nmulti::separated_list1(trailing_space(nbyte::tag(",")), |s| expression(s, ctx))(s)?;
    let (s2, _) = nbyte::tag(")")(s1)?;
    let (s3, _) = empty(s2)?;

    if exprs.len() == 1 {
        Ok((s3, exprs.pop().unwrap()))
    } else {
        Ok((
            s3,
            ast::Expr::new_untyped(
                ast::ExprNode::Tuple(ctx.push_expr_many(exprs.into_iter())),
                ctx.meta(s, s2),
            ),
        ))
    }
}

fn prefix_expression<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    // Parenthesed
    if let (s1, Some(_)) = ncomb::opt(nbyte::tag("("))(s)? {
        let (s1, _) = empty(s1)?;
        expression_parenthesed(s1, ctx)
    } else if let (s1, Some(_)) = ncomb::opt(nbyte::tag("if"))(s)? {
        let (s1, _) = empty(s1)?;
        if_expr(s1, ctx)
    } else if let (s1, Some(_)) = ncomb::opt(nbyte::tag("case"))(s)? {
        let (s1, _) = empty(s1)?;
        case_expr(s1, ctx)
    } else if let (s1, Some(_)) = ncomb::opt(nbyte::tag("let"))(s)? {
        let (s1, _) = empty(s1)?;
        let_in_expr(s1, ctx)
    } else {
        expression_item(s, ctx)
    }
}

fn build_infix_expr<'s>(
    lhs: ast::Expr<'s>,
    op_id: &'s str,
    op_meta: ast::Meta,
    ctx: &mut Context<'s, '_>,
    rest: &'s str,
    lhs_begin: &'s str,
) -> EResult<'s> {
    let p_right = ctx.priority_right(op_id, &op_meta);
    let (s1, rhs) = experssion_with_precedence(rest, ctx, p_right)?;
    let callee = resolve_identifier(op_id, op_meta, ctx);
    let args = [lhs, rhs];
    Ok((
        s1,
        ast::Expr::new_untyped(
            ast::ExprNode::Call {
                callee: ctx.push_expr(callee),
                args: ctx.push_expr_many(args.into_iter()),
            },
            ctx.meta(lhs_begin, s1),
        ),
    ))
}

fn expression_series<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, Vec<ast::Expr<'s>>> {
    nmulti::separated_list1(trailing_space(nbyte::tag(",")), |s| expression(s, ctx))(s)
}

fn build_application_expr<'s>(
    lhs: ast::Expr<'s>,
    ctx: &mut Context<'s, '_>,
    rest: &'s str,
    lhs_begin: &'s str,
) -> EResult<'s> {
    let (s1, arguments) = expression_series(rest, ctx)?;
    Ok((
        s1,
        ast::Expr::new_untyped(
            ast::ExprNode::Call {
                callee: ctx.push_expr(lhs),
                args: ctx.push_expr_many(arguments.into_iter()),
            },
            ctx.meta(lhs_begin, s1),
        ),
    ))
}

fn add_type<'s>(mut lhs: ast::Expr<'s>, ctx: &mut Context<'s, '_>, rest: &'s str) -> EResult<'s> {
    let (s1, type_) = ctx.with_allow_wildcard_in_type(|ctx| type_(rest, ctx))?;
    lhs.type_ = Some(type_);
    Ok((s1, lhs))
}

fn expression_boundary<'s>(s: &'s str) -> PResult<'s, &'s str> {
    nbr::alt((
        nbyte::tag("|"),
        nbyte::tag("then"),
        nbyte::tag("else"),
        nbyte::tag("of"),
        nbyte::tag("in"),
        nbyte::tag("let"),
        nbyte::tag("data"),
        nbyte::tag("->"),
        nbyte::tag(")"),
        nbyte::tag(","),
    ))(s)
}

fn experssion_with_precedence<'s>(
    s: &'s str,
    ctx: &mut Context<'s, '_>,
    prec: OpPriority,
) -> EResult<'s> {
    let lhs_begin = s;
    let (mut s, mut lhs) = prefix_expression(s, ctx)?;
    while s.len() > 0 {
        if let (_, Some(_)) = ncomb::opt(expression_boundary)(s)? {
            break;
        }
        // Annotate `lhs` with some user provided type
        if let (s1, Some(_)) = ncomb::opt(nbyte::tag(":"))(s)? {
            let (s1, _) = empty(s1)?;
            (s, lhs) = add_type(lhs, ctx, s1)?;
            continue;
        }

        // Infix operator
        let (s1, r) = ncomb::opt(|s| infix_identifier_str(s, ctx))(s)?;
        if let Some((op_id, op_meta)) = r {
            if ctx.priority_left(op_id, &op_meta) < prec {
                break;
            }
            (s, lhs) = build_infix_expr(lhs, op_id, op_meta, ctx, s1, lhs_begin)?;
            continue;
        }

        // Function application
        (s, lhs) = build_application_expr(lhs, ctx, s1, lhs_begin)?;
    }

    Ok((s, lhs))
}

fn expression<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> EResult<'s> {
    experssion_with_precedence(s, ctx, 0)
}

fn function_signature<'s>(
    s: &'s str,
    ctx: &mut Context<'s, '_>,
) -> PResult<'s, (&'s str, ast::CallableType<'s>, usize, ast::Meta)> {
    let (s1, _) = trailing_space(nbyte::tag("let"))(s)?;
    let (s1, _) = trailing_space(nbyte::tag("fn"))(s1)?;

    let (s1, (f_name, _)) = function_name_identifier(s1, ctx)?;

    let (s2, (generic_params, _)) = generic_params_decl(s1, ctx)?;
    ctx.set_generic_params(generic_params);

    let (s3, (_, _)) = function_params(s2, ctx)?;
    let (s3, _) = trailing_space(nbyte::tag(":"))(s3)?;

    let (s4, t) = type_callable_type(s3, ctx)?;

    let (s5, _) = trailing_space(nbyte::tag("="))(s4)?;
    let (s5, _) = nbyte::tag("..")(s5)?;
    let (s6, _) = empty(s5)?;

    Ok((s6, (f_name, t, ctx.generic_param_cnt(), ctx.meta(s, s5))))
}

fn function<'s>(
    s: &'s str,
    ctx: &mut Context<'s, '_>,
) -> PResult<'s, (&'s str, ast::Function<'s>)> {
    let (s1, _) = trailing_space(nbyte::tag("let"))(s)?;
    let (s1, _) = trailing_space(nbyte::tag("fn"))(s1)?;

    let (s1, (f_name, _)) = function_name_identifier(s1, ctx)?;

    let (s2, (generic_params, _)) = generic_params_decl(s1, ctx)?;
    ctx.set_generic_params(generic_params);

    let (s3, (params, param_metas)) = function_params(s2, ctx)?;

    let (s4, f_type) = if let (s4, Some(_)) = ncomb::opt(nbyte::tag(":"))(s3)? {
        let (s4, _) = empty(s4)?;
        let (s4, t) = type_callable_type(s4, ctx)?;
        (s4, Some(t))
    } else {
        (s3, None)
    };

    let (s5, _) = trailing_space(nbyte::tag("="))(s4)?;

    let (local_cnt, r) = ctx.with_function_scope(|ctx| {
        ctx.with_scope(|ctx| {
            params.iter().for_each(|p| {
                ctx.define_local(p);
            });
            expression(s5, ctx)
        })
    });
    let (s6, body) = r?;

    let f = ast::Function {
        type_: f_type,
        var_cnt: ctx.generic_param_cnt(),
        local_cnt,
        arity: params.len(),
        body: ctx.push_expr(body),
        meta: ctx.meta(s, s6),
        param_metas,
        captures: vec![],
        captures_meta: vec![],
    };

    Ok((s6, (f_name, f)))
}

use std::sync::Arc;
fn parse_ast_<'s>(s: &'s str, ctx: &mut Context<'s, '_>) -> PResult<'s, ()> {
    let (mut s, _) = empty(s)?;

    while s.len() > 0 {
        let (s1, r) = ncomb::opt(|s| data(s, ctx))(s)?;
        if let Some((data_name, data, constructors)) = r {
            ctx.symbols.add_data(data_name, data);
            constructors
                .into_iter()
                .for_each(|c| ctx.symbols.add_constructor(c.name, c));
            s = s1;
            continue;
        }

        if let (s1, Some(_)) = ncomb::opt(|s| infix_decl(s, ctx))(s)? {
            s = s1;
            continue;
        }

        let (s1, r) = ncomb::opt(|s| function(s, ctx))(s)?;
        if let Some((f_name, f)) = r {
            s = s1;
            ctx.symbols.add_function(f_name.to_string(), f);
            continue;
        }

        // Error
        return Err(nom::Err::Error(nom::error::Error::new(
            s,
            nom::error::ErrorKind::Alt,
        )));
    }

    Ok((s, ()))
}

pub fn parse_ast<'s>(
    s: &'s str,
    src: Arc<String>,
    file_name: Arc<String>,
    symbols: &mut ast::Symbols<'s>,
    ast_nodes: &mut ast::ExprHeap<'s>,
    e: &mut errors::ManyError,
    precedence_table: &mut PrecedenceTable<'s>,
) -> Result<(), errors::ParseError> {
    let mut ctx = Context::new(file_name, src, ast_nodes, e, precedence_table, symbols);

    let r = parse_ast_(s, &mut ctx).finish();

    if let Err(e) = r {
        let m = ctx.meta(e.input, e.input);
        return Err(errors::ParseError::new(&m));
    }
    Ok(())
}

pub fn parse_builtin_function_signatures<'s>(
    s: &'s str,
    src: Arc<String>,
    file_name: Arc<String>,
    symbols: &mut ast::Symbols<'s>,
) {
    let mut ast_ndoes = ast::ExprHeap::new();
    let mut e = errors::ManyError::new();
    let mut ptable = PrecedenceTable::new();
    let mut ctx = Context::new(file_name, src, &mut ast_ndoes, &mut e, &mut ptable, symbols);

    let (s, _) = empty(s).unwrap();
    let (_, sigs) = nmulti::many1(|s| function_signature(s, &mut ctx))(s).unwrap();

    for (name, type_, var_cnt, meta) in sigs.into_iter() {
        symbols.add_builtin(
            name,
            ast::BuiltinFunction {
                var_cnt,
                type_,
                meta,
            },
        )
    }
}
