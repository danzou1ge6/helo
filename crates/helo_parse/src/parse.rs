use crate::ast;
use crate::errors;

use nom::branch as nbr;
use nom::bytes::complete as nbyte;
use nom::character::complete as nchar;
use nom::combinator as ncomb;
use nom::error::context as nom_context;
use nom::multi as nmulti;
use nom::sequence as nseq;
use nom::Finish;

mod context;
mod lower_tast;
pub mod tast;

use context::OpPriority;
pub use context::{Precedence, PrecedenceTable};

pub use lower_tast::lower_symbols;

#[derive(Clone)]
struct Source {
    file_name: std::sync::Arc<String>,
    src: std::sync::Arc<String>,
}

impl Source {
    pub fn meta(&self, begin: &str, end: &str) -> ast::Meta {
        let span = (self.src.len() - begin.len(), begin.len() - end.len());
        ast::Meta {
            span,
            file_name: self.file_name.clone(),
            src: self.src.clone(),
        }
    }
}

type NomE<'s> = nom::error::VerboseError<&'s str>;
type PResult<'s, T> = nom::IResult<&'s str, T, NomE<'s>>;
type MResult<'s, U> = PResult<'s, (U, ast::Meta)>;
type EResult<'s> = PResult<'s, tast::Expr<'s>>;
type SResult<'s> = PResult<'s, tast::Stmt<'s>>;

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

fn trailing_space_tag<'a>(tag: &'a str) -> impl FnMut(&'a str) -> PResult<'a, &'a str> {
    move |s| {
        let (s1, r) = nbyte::tag(tag)(s)?;
        let (s1, _) = empty(s1)?;
        Ok((s1, r))
    }
}

// const symbolic_keywords: [&'static str; 3] = ["|", ")", ","];
const ALPHABETICAL_KEYWORDS: [&'static str; 8] =
    ["then", "else", "of", "case", "let", "fn", "in", "data"];

/// An identifier that is made purely of a set of symbols. NOTE that the `_a` suffix means trailing spaces
/// are not consumed
fn symbolic_identifier_str_a<'s>(s: &'s str) -> PResult<'s, &'s str> {
    let parse = |s| {
        let (s1, id) = nbyte::is_a("~!@#$%^&*<>=+-./")(s)?;
        Ok((s1, id))
    };
    nom_context("symbolic identifier", parse)(s)
}

/// An identifier that is not started with a number, and doesn't contain any symbol
fn alphabetic_identifier_str_a<'s>(s: &'s str) -> PResult<'s, &'s str> {
    let parse = |s| {
        let (s1, _) = nchar::none_of("0123456789~/<>,.:\"'[]{}|\\+=-()*&^%$#@!` \r\n")(s)?;
        let (s2, left): (&str, Option<&str>) =
            ncomb::opt(nbyte::is_not("~/<>,.:\"'[]{}|\\+=-()*&^%$#@!` \r\n"))(s1)?;

        let (s2, id) = if let Some(left) = left {
            (s2, &s[0..left.len() + 1])
        } else {
            (s2, &s[0..1])
        };

        if ALPHABETICAL_KEYWORDS.contains(&id) {
            Err(nom::Err::Error(nom::error::VerboseError {
                errors: vec![(
                    s,
                    nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Alpha),
                )],
            }))
        } else {
            Ok((s2, id))
        }
    };
    nom_context("alphabetic identifier", parse)(s)
}

/// An `alphabetic_identifier` that is surrounded by "`", indicating that it's used as an infix operator
fn infix_alphabetic_identifier_str_a<'s>(s: &'s str) -> PResult<'s, &'s str> {
    let parse = |s| {
        let (s1, _) = nbyte::tag("`")(s)?;
        let (s2, id_mid) = alphabetic_identifier_str_a(s1)?;
        let (s3, _) = nbyte::tag("`")(s2)?;
        Ok((s3, &s1[0..id_mid.len()]))
    };
    nom_context("infix alphabetic identifier", parse)(s)
}

/// An identifier that is used as an infix operator
fn infix_identifier_str_a<'s>(s: &'s str) -> PResult<'s, &'s str> {
    nom_context(
        "infix identifier",
        nbr::alt((symbolic_identifier_str_a, infix_alphabetic_identifier_str_a)),
    )(s)
}

fn infix_identifier_str<'s>(s: &'s str) -> PResult<'s, &'s str> {
    trailing_space(infix_identifier_str_a)(s)
}

/// Like `alphabetic_identifier_str_a`, but removes trailing space
fn alphabetic_identifier_str<'s>(s: &'s str) -> PResult<'s, &'s str> {
    let (s1, r) = alphabetic_identifier_str_a(s)?;
    let (s2, _) = empty(s1)?;
    Ok((s2, r))
}

/// Like `alphabetic_identifier_str_a`, but removes trailing space
fn alphabetic_identifier_str_with_meta<'s>(
    s: &'s str,
    ctx: &Source,
) -> PResult<'s, (&'s str, ast::Meta)> {
    let (s1, r) = alphabetic_identifier_str_a(s)?;
    let (s2, _) = empty(s1)?;
    Ok((s2, (r, ctx.meta(s, s1))))
}

/// A number literal, which can either be float or int. The `_expr` suffix indicates that it parses an expression.
fn num_literal_expr<'s>(s: &'s str, ctx: &Source) -> EResult<'s> {
    let (s1, (c, m)) = trailing_space(|s| num_literal_a(s, ctx))(s)?;
    Ok((s1, tast::Expr::new_untyped(tast::ExprNode::Constant(c), m)))
}

fn num_literal_a<'s>(s: &'s str, ctx: &Source) -> MResult<'s, tast::Constant<'s>> {
    let parse = |s| {
        let (s1, d1): (_, &str) = nchar::digit1(s)?;
        let (s2, d2) = ncomb::opt(nseq::preceded(nbyte::tag("."), nchar::digit1))(s1)?;
        if let Some(d2) = d2 {
            Ok((
                s2,
                (
                    tast::Constant::Float(&s[0..d1.len() + 1 + d2.len()]),
                    ctx.meta(s, s2),
                ),
            ))
        } else {
            Ok((
                s2,
                (tast::Constant::Int(d1.parse().unwrap()), ctx.meta(s, s2)),
            ))
        }
    };
    nom_context("numerical literal", parse)(s)
}

/// Like `num_literal_expr`
fn string_literal_expr<'s>(s: &'s str, ctx: &Source) -> EResult<'s> {
    let (s1, (c, m)) = trailing_space(|s| string_literal_a(s, ctx))(s)?;
    Ok((s1, tast::Expr::new_untyped(tast::ExprNode::Constant(c), m)))
}

fn char_literal_a<'s>(s: &'s str, ctx: &Source) -> MResult<'s, tast::Constant<'s>> {
    let parse = |s| {
        let (s1, _) = nbyte::tag("'")(s)?;
        let (s2, string) = nbyte::take_until("'")(s1)?;
        let (s3, _) = nbyte::tag("'")(s2)?;
        let (s3, _) = empty(s3)?;
        Ok((s3, (tast::Constant::Char(string), ctx.meta(s, s3))))
    };
    nom_context("char literal", parse)(s)
}

fn char_literal_expr<'s>(s: &'s str, ctx: &Source) -> EResult<'s> {
    let (s1, (c, meta)) = char_literal_a(s, ctx)?;

    Ok((
        s1,
        tast::Expr::new_untyped(tast::ExprNode::Constant(c), meta),
    ))
}

fn string_literal_a<'s>(s: &'s str, ctx: &Source) -> MResult<'s, tast::Constant<'s>> {
    let parse = |s| {
        let (s1, _) = nbyte::tag("\"")(s)?;
        let (s2, string) = nbyte::take_until("\"")(s1)?;
        let (s3, _) = nbyte::tag("\"")(s2)?;
        Ok((s3, (tast::Constant::Str(string), ctx.meta(s, s3))))
    };
    nom_context("string literal", parse)(s)
}

fn if_expr<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let parse = |s| {
        let (s2, test) = expression(s, ctx, precedence_table, generic_params)?;

        let (s3, _) = trailing_space_tag("then")(s2)?;
        let (s4, then) = expression(s3, ctx, precedence_table, generic_params)?;

        let (s5, _) = trailing_space_tag("else")(s4)?;
        let (s6, else_) = expression(s5, ctx, precedence_table, generic_params)?;

        Ok((
            s6,
            tast::Expr::new_untyped(
                tast::ExprNode::IfElse {
                    test: Box::new(test),
                    then: Box::new(then),
                    else_: Box::new(else_),
                },
                ctx.meta(s, s6),
            ),
        ))
    };
    nom_context("if-expression", parse)(s)
}

/// A constructor pattern, like `Some 1`
fn pattern_construct<'s>(
    s: &'s str,
    begin: &'s str,
    constructor: &'s str,
    ctx: &Source,
) -> PResult<'s, tast::Pattern<'s>> {
    let parse = |s| {
        let (s1, args) = nmulti::separated_list0(trailing_space_tag(","), |s| pattern(s, ctx))(s)?;
        Ok((
            s1,
            tast::Pattern::Construct(constructor, args, ctx.meta(begin, s1)),
        ))
    };
    nom_context("construct-pattern", parse)(s)
}

/// A literal pattern
fn pattern_literal<'s>(s: &'s str, ctx: &Source) -> PResult<'s, tast::Pattern<'s>> {
    let parse = |s| {
        let (s1, (value, meta)) = nbr::alt((
            |s| string_literal_a(s, ctx),
            |s| char_literal_a(s, ctx),
            |s| num_literal_a(s, ctx),
        ))(s)?;
        let (s1, _) = empty(s1)?;
        Ok((s1, tast::Pattern::Literal(value, meta)))
    };
    nom_context("literal pattern", parse)(s)
}

fn parenthesed_pattern<'s>(
    s: &'s str,
    begin: &'s str,
    ctx: &Source,
) -> PResult<'s, tast::Pattern<'s>> {
    let (s1, mut pats) = nmulti::separated_list1(trailing_space_tag(","), |s| pattern(s, ctx))(s)?;
    let (s2, _) = nbyte::tag(")")(s1)?;
    let (s3, _) = empty(s2)?;

    if pats.len() == 1 {
        Ok((s3, pats.pop().unwrap()))
    } else {
        Ok((s3, tast::Pattern::Tuple(pats, ctx.meta(begin, s2))))
    }
}

/// A pattern
fn pattern<'s>(s: &'s str, ctx: &Source) -> PResult<'s, tast::Pattern<'s>> {
    let parse = |s| {
        if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("("))(s)? {
            parenthesed_pattern(s1, s, ctx)
        } else if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("mut"))(s)? {
            let (s2, id) = alphabetic_identifier_str(s1)?;
            Ok((s2, tast::Pattern::Bind(id, true, ctx.meta(s, s2))))
        } else if let (s1, Some(constructor)) = ncomb::opt(alphabetic_identifier_str)(s)? {
            if !constructor.chars().next().unwrap().is_uppercase() {
                Ok((s1, tast::Pattern::Bind(constructor, false, ctx.meta(s, s1))))
            } else {
                pattern_construct(s1, s, constructor, ctx)
            }
        } else {
            pattern_literal(s, ctx)
        }
    };
    nom_context("pattern", parse)(s)
}

fn case_arm<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> PResult<'s, tast::CaseArm<'s>> {
    let parse = |s| {
        let (s1, pat) = pattern(s, ctx)?;

        let (s2, guard) = if let (s2, Some(_)) = ncomb::opt(trailing_space_tag("if"))(s1)? {
            let (s3, guard) = expression(s2, ctx, precedence_table, generic_params)?;
            (s3, Some(guard))
        } else {
            (s1, None)
        };

        let (s2, _) = trailing_space_tag("->")(s2)?;
        let (s3, result) = expression(s2, ctx, precedence_table, generic_params)?;
        Ok((
            s3,
            tast::CaseArm {
                pattern: pat,
                guard,
                result,
            },
        ))
    };
    nom_context("case-of arm", parse)(s)
}

fn case_expr<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let parse = |s| {
        let (s2, operand) = expression(s, ctx, precedence_table, generic_params)?;

        let (s3, _) = trailing_space_tag("of")(s2)?;

        let (s4, arms) = nmulti::separated_list1(trailing_space_tag("|"), |s| {
            case_arm(s, ctx, precedence_table, generic_params)
        })(s3)?;
        Ok((
            s4,
            tast::Expr::new_untyped(
                tast::ExprNode::Case {
                    operand: Box::new(operand),
                    arms,
                },
                ctx.meta(s, s4),
            ),
        ))
    };
    nom_context("case-of expression", parse)(s)
}

fn let_in_bind_expr<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let parse = |s| {
        let (s1, pattern) = pattern(s, ctx)?;

        let (s1, _) = trailing_space_tag("=")(s1)?;

        let (s2, value) = expression(s1, ctx, precedence_table, generic_params)?;

        let (s3, _) = trailing_space_tag("in")(s2)?;
        let (s4, in_) = expression(s3, ctx, precedence_table, generic_params)?;
        Ok((
            s4,
            tast::Expr::new_untyped(
                tast::ExprNode::LetPatIn {
                    bind: pattern,
                    value: Box::new(value),
                    in_: Box::new(in_),
                },
                ctx.meta(s, s4),
            ),
        ))
    };
    nom_context("let-in expression", parse)(s)
}

fn function_params<'s>(s: &'s str, ctx: &Source) -> PResult<'s, (Vec<&'s str>, Vec<ast::Meta>)> {
    let parse = |s| {
        let (s1, r) = nmulti::separated_list0(trailing_space_tag(","), |s| {
            alphabetic_identifier_str_with_meta(s, ctx)
        })(s)?;
        let (params, meta) = r.into_iter().unzip();
        Ok((s1, (params, meta)))
    };
    nom_context("function parameters", parse)(s)
}

fn let_in_closure_expr<'s>(
    s: &'s str,
    pure: bool,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let parse = |s| {
        let (s1, id) = alphabetic_identifier_str(s)?;

        let (s2, params) = nmulti::separated_list0(trailing_space_tag(","), |s| {
            alphabetic_identifier_str_with_meta(s, ctx)
        })(s1)?;
        let (params, params_meta): (Vec<_>, Vec<_>) = params.into_iter().unzip();

        let (s3, _) = trailing_space_tag("=")(s2)?;

        let (s4, body) = expression(s3, ctx, precedence_table, generic_params)?;

        let f = tast::Function {
            type_: None,
            var_cnt: 0,
            body: Box::new(body),
            meta: ctx.meta(s, s4),
            params,
            param_metas: params_meta,
            pure,
        };

        let (s5, _) = trailing_space_tag("in")(s4)?;
        let (s6, in_) = expression(s5, ctx, precedence_table, generic_params)?;

        let closure_expr = tast::Expr::new_untyped(
            tast::ExprNode::LetFnIn {
                identifier: id,
                f,
                in_: Box::new(in_),
            },
            ctx.meta(s, s6),
        );

        Ok((s6, closure_expr))
    };
    nom_context("let-fn-in expression", parse)(s)
}

fn let_in_expr<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("fn"))(s)? {
        let_in_closure_expr(s1, true, ctx, precedence_table, generic_params)
    } else if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("routine"))(s)? {
        let_in_closure_expr(s1, false, ctx, precedence_table, generic_params)
    } else {
        let_in_bind_expr(s, ctx, precedence_table, generic_params)
    }
}

fn generic_param_a<'s>(s: &'s str, ctx: &Source) -> MResult<'s, &'s str> {
    let (s1, _) = nbyte::tag("'")(s)?;
    let (s2, name) = nchar::alphanumeric1(s1)?;
    Ok((s2, (name, ctx.meta(s, s2))))
}

fn generic_params_decl<'s>(
    s: &'s str,
    ctx: &Source,
) -> PResult<'s, (Vec<&'s str>, Vec<ast::Meta>)> {
    let parse = |s| {
        let (s1, generics) = nmulti::separated_list0(
            trailing_space_tag(","),
            trailing_space(|s| generic_param_a(s, ctx)),
        )(s)?;
        Ok((s1, generics.into_iter().unzip()))
    };
    nom_context("generic parameters", parse)(s)
}

fn type_generic<'s>(
    s: &'s str,
    ctx: &Source,
    generic_params: &Vec<&'s str>,
) -> PResult<'s, tast::Type<'s>> {
    let parse = |s| {
        let (s1, (template, template_meta)) = alphabetic_identifier_str_with_meta(s, ctx)?;

        macro_rules! return_constant {
            ($ty:ident) => {
                return Ok((
                    s1,
                    tast::Type {
                        node: tast::TypeNode::Primitive(ast::PrimitiveType::$ty),
                        meta: ctx.meta(s, s1),
                    },
                ))
            };
        }

        match template {
            "Int" => return_constant!(Int),
            "Float" => return_constant!(Float),
            "Str" => return_constant!(Str),
            "Bool" => return_constant!(Bool),
            "Char" => return_constant!(Char),
            _ => {}
        }

        if template == "_" {
            return Ok((
                s1,
                tast::Type {
                    node: tast::TypeNode::WildCard,
                    meta: ctx.meta(s, s1),
                },
            ));
        }

        let (s4, args, meta) = if let (s2, Some(_)) = ncomb::opt(trailing_space_tag("["))(s1)? {
            let (s3, args) = nmulti::separated_list1(trailing_space_tag(","), |s| {
                type_(s, ctx, generic_params)
            })(s2)?;
            let (s4, _) = nbyte::tag("]")(s3)?;
            let (s5, _) = empty(s4)?;
            (s5, args, ctx.meta(s, s4))
        } else {
            (s1, vec![], template_meta)
        };

        Ok((
            s4,
            tast::Type {
                node: tast::TypeNode::Generic(template, args),
                meta,
            },
        ))
    };
    nom_context("generic type", parse)(s)
}

fn type_callable<'s>(
    s: &'s str,
    ctx: &Source,
    generic_params: &Vec<&'s str>,
) -> PResult<'s, tast::Type<'s>> {
    let parse = |s| {
        let (s1, _) = trailing_space_tag("[")(s)?;
        let (s2, params) =
            nmulti::separated_list1(trailing_space_tag(","), |s| type_(s, ctx, generic_params))(
                s1,
            )?;
        let (s3, _) = trailing_space_tag("]")(s2)?;

        let (s3, _) = trailing_space_tag("->")(s3)?;
        let (s4, ret) = type_(s3, ctx, generic_params)?;

        Ok((
            s4,
            tast::Type {
                node: tast::TypeNode::Callable(tast::CallableType {
                    params,
                    ret: Box::new(ret),
                }),
                meta: ctx.meta(s, s4),
            },
        ))
    };
    nom_context("callable type", parse)(s)
}

fn type_callable_type<'s>(
    s: &'s str,
    ctx: &Source,
    generic_params: &Vec<&'s str>,
) -> PResult<'s, tast::CallableType<'s>> {
    let parse = |s| {
        let (s1, _) = trailing_space_tag("[")(s)?;
        let (s2, params) =
            nmulti::separated_list1(trailing_space_tag(","), |s| type_(s, ctx, generic_params))(
                s1,
            )?;
        let (s3, _) = trailing_space_tag("]")(s2)?;

        let (s3, _) = trailing_space_tag("->")(s3)?;
        let (s4, ret) = type_(s3, ctx, generic_params)?;

        Ok((
            s4,
            tast::CallableType {
                params,
                ret: Box::new(ret),
            },
        ))
    };
    nom_context("callable type", parse)(s)
}

fn type_unit<'s>(s: &'s str, ctx: &Source) -> PResult<'s, tast::Type<'s>> {
    let (s1, _) = nbyte::tag("()")(s)?;
    let (s2, _) = empty(s1)?;
    Ok((
        s2,
        tast::Type {
            node: tast::TypeNode::Unit,
            meta: ctx.meta(s, s1),
        },
    ))
}

fn type_parenthesed<'s>(
    s: &'s str,
    ctx: &Source,
    generic_params: &Vec<&'s str>,
) -> PResult<'s, tast::Type<'s>> {
    let parse = |s| {
        let (s2, args) =
            nmulti::separated_list0(trailing_space_tag(","), |s| type_(s, ctx, generic_params))(s)?;
        let (s3, _) = nbyte::tag(")")(s2)?;
        let (s4, _) = empty(s3)?;

        Ok((
            s4,
            tast::Type {
                node: if args.len() != 0 {
                    tast::TypeNode::Tuple(args)
                } else {
                    tast::TypeNode::Unit
                },
                meta: ctx.meta(s, s3),
            },
        ))
    };
    nom_context("tuple type", parse)(s)
}

fn type_var<'s>(
    s: &'s str,
    ctx: &Source,
    generic_params: &Vec<&'s str>,
) -> PResult<'s, tast::Type<'s>> {
    let (s1, id) = alphabetic_identifier_str(s)?;

    if let Some(idx) = generic_params.iter().position(|p| *p == id) {
        Ok((
            s1,
            tast::Type {
                node: tast::TypeNode::Var(idx.into()),
                meta: ctx.meta(s, s1),
            },
        ))
    } else {
        Err(nom::Err::Error(nom::error::VerboseError {
            errors: vec![(
                s,
                nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Verify),
            )],
        }))
    }
}

fn type_<'s>(
    s: &'s str,
    ctx: &Source,
    generic_params: &Vec<&'s str>,
) -> PResult<'s, tast::Type<'s>> {
    if let (s1, Some(_)) = ncomb::opt(nbyte::tag("("))(s)? {
        type_parenthesed(s1, ctx, generic_params)
    } else if let (s1, Some(_)) = ncomb::opt(nbyte::tag("["))(s)? {
        type_callable(s1, ctx, generic_params)
    } else if let (s1, Some(_)) = ncomb::opt(nbyte::tag("'"))(s)? {
        type_var(s1, ctx, generic_params)
    } else {
        type_generic(s, ctx, generic_params)
    }
}

fn constructor<'s>(
    s: &'s str,
    ctx: &Source,
    data_name: &'s str,
    generic_params: &Vec<&'s str>,
) -> PResult<'s, tast::Constructor<'s>> {
    let parse = |s| {
        let (s1, name) = alphabetic_identifier_str(s)?;

        let (s2, params) =
            nmulti::separated_list0(trailing_space_tag(","), |s| type_(s, ctx, generic_params))(
                s1,
            )?;
        Ok((
            s2,
            tast::Constructor {
                name,
                params,
                belongs_to: data_name,
                meta: ctx.meta(s, s2),
            },
        ))
    };
    nom_context("constructor", parse)(s)
}

fn data<'s>(
    s: &'s str,
    ctx: &Source,
) -> PResult<'s, (&'s str, ast::Data<'s>, Vec<tast::Constructor<'s>>)> {
    let parse = |s| {
        let (s1, (data_name, _)) = alphabetic_identifier_str_with_meta(s, ctx)?;

        let (s3, generic_params, generic_metas) =
            if let (s2, Some(_)) = ncomb::opt(trailing_space_tag("["))(s1)? {
                let (s3, (generic_params, generic_metas)) = generic_params_decl(s2, ctx)?;
                let (s3, _) = trailing_space_tag("]")(s3)?;
                (s3, generic_params, generic_metas)
            } else {
                (s1, Vec::new(), Vec::new())
            };

        let kind_arity = generic_params.len();

        let (s4, _) = trailing_space_tag("=")(s3)?;

        let (s5, constructors) = nmulti::separated_list1(trailing_space_tag("|"), |s| {
            constructor(s, ctx, data_name, &generic_params)
        })(s4)?;

        let meta = ctx.meta(s, s5);

        let data = ast::Data {
            name: data_name,
            kind_arity,
            constructors: constructors.iter().map(|c| c.name).collect(),
            meta,
            generic_metas,
        };

        Ok((s5, (data_name, data, constructors)))
    };
    nom_context("data", parse)(s)
}

fn function_name_identifier_a<'s>(s: &'s str, ctx: &Source) -> MResult<'s, &'s str> {
    let parse = |s| {
        let (s1, id) = nbr::alt((symbolic_identifier_str_a, alphabetic_identifier_str_a))(s)?;
        Ok((s1, (id, ctx.meta(s, s1))))
    };
    nom_context("function name identifier", parse)(s)
}

fn function_name_identifier<'s>(s: &'s str, ctx: &Source) -> MResult<'s, &'s str> {
    trailing_space(|s| function_name_identifier_a(s, ctx))(s)
}

fn identifier_expr<'s>(s: &'s str, ctx: &Source) -> EResult<'s> {
    let (s1, (id, meta)) = trailing_space(|s| function_name_identifier_a(s, ctx))(s)?;
    Ok((
        s1,
        tast::Expr::new_untyped(tast::ExprNode::Identifier(id), meta),
    ))
}

fn expression_item<'s>(s: &'s str, ctx: &Source) -> EResult<'s> {
    nom_context(
        "identifier, number literal, char literal or string literal",
        nbr::alt((
            |s| identifier_expr(s, ctx),
            |s| num_literal_expr(s, ctx),
            |s| string_literal_expr(s, ctx),
            |s| char_literal_expr(s, ctx),
        )),
    )(s)
}

fn expression_parenthesed<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let (s1, mut exprs) = nmulti::separated_list0(trailing_space_tag(","), |s| {
        expression(s, ctx, precedence_table, generic_params)
    })(s)?;
    let (s2, _) = nbyte::tag(")")(s1)?;
    let (s3, _) = empty(s2)?;

    if exprs.len() == 0 {
        Ok((
            s3,
            tast::Expr::new_untyped(tast::ExprNode::Unit, ctx.meta(s, s2)),
        ))
    } else if exprs.len() == 1 {
        Ok((s3, exprs.pop().unwrap()))
    } else {
        Ok((
            s3,
            tast::Expr::new_untyped(tast::ExprNode::Tuple(exprs), ctx.meta(s, s2)),
        ))
    }
}

fn prefix_expression<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    // Parenthesed
    if let (s1, Some(_)) = ncomb::opt(nbyte::tag("("))(s)? {
        let (s1, _) = empty(s1)?;
        expression_parenthesed(s1, ctx, precedence_table, generic_params)
    } else if let (s1, Some(_)) = ncomb::opt(nbyte::tag("if"))(s)? {
        let (s1, _) = empty(s1)?;
        if_expr(s1, ctx, precedence_table, generic_params)
    } else if let (s1, Some(_)) = ncomb::opt(nbyte::tag("case"))(s)? {
        let (s1, _) = empty(s1)?;
        case_expr(s1, ctx, precedence_table, generic_params)
    } else if let (s1, Some(_)) = ncomb::opt(nbyte::tag("let"))(s)? {
        let (s1, _) = empty(s1)?;
        let_in_expr(s1, ctx, precedence_table, generic_params)
    } else if let (s1, Some(_)) = ncomb::opt(nbyte::tag("begin"))(s)? {
        let (s1, _) = empty(s1)?;
        seq_expr(s1, ctx, precedence_table, generic_params)
    } else {
        expression_item(s, ctx)
    }
}

fn build_infix_expr<'s>(
    lhs: tast::Expr<'s>,
    op_id: &'s str,
    op_meta: ast::Meta,
    ctx: &Source,
    rest: &'s str,
    lhs_begin: &'s str,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let p_right = precedence_table.priority_right(op_id);
    let (s1, rhs) =
        experssion_with_precedence(rest, ctx, p_right, precedence_table, generic_params)?;
    let callee = tast::Expr::new_untyped(tast::ExprNode::Identifier(op_id), op_meta);

    Ok((
        s1,
        tast::Expr::new_untyped(
            if op_id == "=" {
                tast::ExprNode::Assign(Box::new(lhs), Box::new(rhs))
            } else {
                let args = [lhs, rhs];
                tast::ExprNode::Apply {
                    callee: Box::new(callee),
                    args: args.to_vec(),
                }
            },
            ctx.meta(lhs_begin, s1),
        ),
    ))
}

fn expression_series<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> PResult<'s, Vec<tast::Expr<'s>>> {
    nmulti::separated_list1(trailing_space_tag(","), |s| {
        expression(s, ctx, precedence_table, generic_params)
    })(s)
}

fn build_application_expr<'s>(
    lhs: tast::Expr<'s>,
    ctx: &Source,
    rest: &'s str,
    lhs_begin: &'s str,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let (s1, arguments) = expression_series(rest, ctx, precedence_table, generic_params)?;
    Ok((
        s1,
        tast::Expr::new_untyped(
            tast::ExprNode::Apply {
                callee: Box::new(lhs),
                args: arguments,
            },
            ctx.meta(lhs_begin, s1),
        ),
    ))
}

fn add_type<'s>(
    mut lhs: tast::Expr<'s>,
    ctx: &Source,
    rest: &'s str,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let (s1, type_) = type_(rest, ctx, generic_params)?;
    lhs.type_ = Some(type_);
    Ok((s1, lhs))
}

fn expression_boundary<'s>(s: &'s str) -> PResult<'s, &'s str> {
    nbr::alt((
        nbyte::tag(";"),
        nbyte::tag("|"),
        nbyte::tag("then"),
        nbyte::tag("else"),
        nbyte::tag("of"),
        nbyte::tag("in"),
        nbyte::tag("let"),
        nbyte::tag("fn"),
        nbyte::tag("data"),
        nbyte::tag("end"),
        nbyte::tag("->"),
        nbyte::tag(")"),
        nbyte::tag(","),
    ))(s)
}

fn experssion_with_precedence<'s>(
    s: &'s str,
    ctx: &Source,
    prec: OpPriority,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let lhs_begin = s;
    let (mut s, mut lhs) = prefix_expression(s, ctx, precedence_table, generic_params)?;
    while s.len() > 0 {
        if let (_, Some(_)) = ncomb::opt(expression_boundary)(s)? {
            break;
        }
        // Annotate `lhs` with some user provided type
        if let (s1, Some(_)) = ncomb::opt(nbyte::tag(":"))(s)? {
            let (s1, _) = empty(s1)?;
            (s, lhs) = add_type(lhs, ctx, s1, generic_params)?;
            continue;
        }

        // Infix operator
        let (s1, r) = ncomb::opt(infix_identifier_str)(s)?;
        if let Some(op_id) = r {
            if precedence_table.priority_left(op_id) < prec {
                break;
            }
            (s, lhs) = build_infix_expr(
                lhs,
                op_id,
                ctx.meta(s, s1),
                ctx,
                s1,
                lhs_begin,
                precedence_table,
                generic_params,
            )?;
            continue;
        }

        // Function application
        (s, lhs) =
            build_application_expr(lhs, ctx, s1, lhs_begin, precedence_table, generic_params)?;
    }

    Ok((s, lhs))
}

fn expression<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    experssion_with_precedence(s, ctx, 0, precedence_table, generic_params)
}

fn decl_stmt<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> SResult<'s> {
    let parse = |s| {
        let (s2, pat) = pattern(s, ctx)?;
        let (s3, _) = trailing_space_tag("=")(s2)?;
        let (s4, value) = expression(s3, ctx, precedence_table, generic_params)?;
        Ok((
            s4,
            tast::Stmt::new(tast::StmtNode::LetDecl(pat, value), ctx.meta(s, s4)),
        ))
    };
    nom_context("variable declaration statement", parse)(s)
}

fn if_stmt<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> SResult<'s> {
    let parse = |s| {
        let (s2, test) = expression(s, ctx, precedence_table, generic_params)?;

        let (s3, _) = trailing_space_tag("then")(s2)?;
        let (s4, then) = expression(s3, ctx, precedence_table, generic_params)?;

        Ok((
            s4,
            tast::Stmt::new(tast::StmtNode::If { test, then }, ctx.meta(s, s4)),
        ))
    };
    nom_context("if-statement", parse)(s)
}

fn while_stmt<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> SResult<'s> {
    let parse = |s| {
        let (s2, test) = expression(s, ctx, precedence_table, generic_params)?;

        let (s3, _) = trailing_space_tag("then")(s2)?;
        let (s4, then) = expression(s3, ctx, precedence_table, generic_params)?;

        Ok((
            s4,
            tast::Stmt::new(tast::StmtNode::While { test, then }, ctx.meta(s, s4)),
        ))
    };
    nom_context("while-statement", parse)(s)
}

fn expr_stmt<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> SResult<'s> {
    let parse = |s| {
        let (s1, expr) = expression(s, ctx, precedence_table, generic_params)?;
        Ok((
            s1,
            tast::Stmt::new(tast::StmtNode::Expr(expr), ctx.meta(s, s1)),
        ))
    };
    nom_context("expression statement", parse)(s)
}

fn stmt<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> SResult<'s> {
    if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("let"))(s)? {
        decl_stmt(s1, ctx, precedence_table, generic_params)
    } else if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("if"))(s)? {
        if_stmt(s1, ctx, precedence_table, generic_params)
    } else if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("while"))(s)? {
        while_stmt(s1, ctx, precedence_table, generic_params)
    } else {
        expr_stmt(s, ctx, precedence_table, generic_params)
    }
}

fn seq_expr<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
    generic_params: &Vec<&'s str>,
) -> EResult<'s> {
    let (s1, mut stmts) = nmulti::separated_list1(trailing_space_tag(";"), |s| {
        stmt(s, ctx, precedence_table, generic_params)
    })(s)?;

    let (s2, result) = if let (s2, Some(_)) = ncomb::opt(trailing_space_tag(";"))(s1)? {
        (s2, None)
    } else {
        if let tast::StmtNode::Expr(_) = &stmts.last().unwrap().node {
            (
                s1,
                Some(match stmts.pop().unwrap().node {
                    tast::StmtNode::Expr(expr) => expr,
                    _ => unreachable!(),
                }),
            )
        } else {
            (s1, None)
        }
    };
    let (s3, _) = trailing_space_tag("end")(s2)?;

    Ok((
        s3,
        tast::Expr::new_untyped(
            tast::ExprNode::Seq(stmts, result.map(|x| Box::new(x))),
            ctx.meta(s, s3),
        ),
    ))
}

fn function_signature<'s>(
    s: &'s str,
    ctx: &Source,
) -> PResult<'s, (&'s str, tast::CallableType<'s>, usize, bool, ast::Meta)> {
    let (s1, keyword) = nbr::alt((trailing_space_tag("fn"), trailing_space_tag("routine")))(s)?;

    let pure = match keyword {
        "fn" => true,
        "routine" => false,
        _ => unreachable!(),
    };

    let (s1, (f_name, _)) = function_name_identifier(s1, ctx)?;

    let (s2, (generic_params, _)) = generic_params_decl(s1, ctx)?;

    let (s3, (_, _)) = function_params(s2, ctx)?;
    let (s3, _) = trailing_space_tag(":")(s3)?;

    let (s4, t) = type_callable_type(s3, ctx, &generic_params)?;

    let (s5, _) = trailing_space_tag("=")(s4)?;
    let (s5, _) = nbyte::tag("..")(s5)?;
    let (s6, _) = empty(s5)?;

    Ok((s6, (f_name, t, generic_params.len(), pure, ctx.meta(s, s5))))
}

fn function<'s>(
    s: &'s str,
    pure: bool,
    ctx: &Source,
    precedence_table: &PrecedenceTable<'s>,
) -> PResult<'s, (&'s str, tast::Function<'s>)> {
    let (s1, (f_name, _)) = function_name_identifier(s, ctx)?;

    let (s2, (generic_params, _)) = generic_params_decl(s1, ctx)?;

    let (s3, (params, param_metas)) = function_params(s2, ctx)?;

    let (s4, f_type) = if let (s4, Some(_)) = ncomb::opt(nbyte::tag(":"))(s3)? {
        let (s4, _) = empty(s4)?;
        let (s4, t) = type_callable_type(s4, ctx, &generic_params)?;
        (s4, Some(t))
    } else {
        (s3, None)
    };

    let (s5, _) = trailing_space_tag("=")(s4)?;

    let (s6, body) = expression(s5, ctx, precedence_table, &generic_params)?;

    let f = tast::Function {
        type_: f_type,
        var_cnt: generic_params.len(),
        body: Box::new(body),
        meta: ctx.meta(s, s6),
        param_metas,
        params,
        pure,
    };

    Ok((s6, (f_name, f)))
}

use std::sync::Arc;
fn parse_ast_<'s>(
    s: &'s str,
    ctx: &Source,
    precedence_table: &mut PrecedenceTable<'s>,
    symbols: &mut tast::Symbols<'s>,
) -> PResult<'s, ()> {
    let parse = |s| {
        let (mut s, _) = empty(s)?;

        while s.len() > 0 {
            if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("data"))(s)? {
                let (s2, (data_name, data, constructors)) = data(s1, ctx)?;
                symbols.add_data(data_name, data);
                constructors
                    .into_iter()
                    .for_each(|c| symbols.add_constructor(c.name, c));
                s = s2;
                continue;
            }

            if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("fn"))(s)? {
                let (s1, (f_name, f)) = function(s1, true, ctx, precedence_table)?;
                s = s1;
                symbols.add_function(f_name.to_string(), f);
                continue;
            }

            if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("routine"))(s)? {
                let (s1, (f_name, f)) = function(s1, false, ctx, precedence_table)?;
                s = s1;
                symbols.add_function(f_name.to_string(), f);
                continue;
            }

            if let (s1, Some(_)) = ncomb::opt(trailing_space_tag("infix"))(s)? {
                let (s1, (id, prece)) = infix_decl(s1)?;
                precedence_table.insert(id, prece);
                s = s1;
                continue;
            }

            // Error
            return Err(nom::Err::Error(nom::error::VerboseError {
                errors: vec![(
                    s,
                    nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Alt),
                )],
            }));
        }

        Ok((s, ()))
    };
    nom_context("data, function or declaration of infix operator", parse)(s)
}

pub fn parse_ast<'s>(
    s: &'s str,
    src: Arc<String>,
    file_name: Arc<String>,
    symbols: &mut tast::Symbols<'s>,
    precedence_table: &mut PrecedenceTable<'s>,
) -> Result<(), errors::ParseError> {
    let src_len = src.len();
    let ctx = Source { file_name, src };
    let r = parse_ast_(s, &ctx, precedence_table, symbols).finish();

    if let Err(e) = r {
        return Err(errors::ParseError::new(
            e,
            ctx.meta(s, s).named_source(),
            src_len,
        ));
    }
    Ok(())
}

pub fn parse_builtin_function_signatures<'s>(
    s: &'s str,
    src: Arc<String>,
    file_name: Arc<String>,
    symbols: &mut ast::Symbols<'s>,
) {
    let ctx = Source { file_name, src };

    let (s, _) = empty(s).unwrap();
    let (_, sigs) = nmulti::many1(|s| function_signature(s, &ctx))(s).unwrap();

    for (name, type_, var_cnt, pure, meta) in sigs.into_iter() {
        symbols.add_builtin(
            name,
            ast::BuiltinFunction {
                var_cnt,
                type_,
                meta,
                pure,
            },
        )
    }
}

pub fn parse_builtin_data<'s>(
    s: &'s str,
    src: Arc<String>,
    file_name: Arc<String>,
    symbols: &mut ast::Symbols<'s>,
) {
    let ctx = Source { file_name, src };
    let (s, _) = empty(s).unwrap();

    let data_complete = |s| {
        let (s, _) = trailing_space_tag("data")(s)?;
        data(s, &ctx)
    };

    let (_, datas) = nmulti::many1(data_complete)(s).unwrap();

    for (data_name, data, constructors) in datas.into_iter() {
        symbols.add_data(data_name, data);
        for constructor in constructors.into_iter() {
            symbols.add_constructor(constructor.name, constructor);
        }
    }
}

pub fn parse_infix_declarations<'s>(s: &'s str, precedence_table: &mut PrecedenceTable<'s>) {
    let (s, _) = empty(s).unwrap();
    let infix_decl_complete = |s| {
        let (s, _) = trailing_space_tag("infix")(s)?;
        infix_decl(s)
    };
    let (_, decls) = nmulti::many1(infix_decl_complete)(s).unwrap();
    for (id, prec) in decls.into_iter() {
        precedence_table.insert(id, prec);
    }
}

fn infix_decl<'s>(s: &'s str) -> PResult<'s, (&'s str, Precedence)> {
    let (s2, id) = infix_identifier_str(s)?;

    let (s3, p_left) = trailing_space(nchar::u32)(s2)?;
    let (s3, p_right) = trailing_space(nchar::u32)(s3)?;

    Ok((s3, (id, Precedence(p_left, p_right))))
}
