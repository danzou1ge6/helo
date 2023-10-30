use miette::{Diagnostic, NamedSource, Report, SourceSpan};
use thiserror::Error;

use crate::ast;

#[derive(Diagnostic, Debug, Error)]
#[error("Can only assign to local mutable variable")]
pub struct OnlyLocalAssign {
    #[source_code]
    pub src: NamedSource,
    #[label("Assign Here")]
    pub span: SourceSpan,
}

impl OnlyLocalAssign {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}
#[derive(Diagnostic, Debug, Error)]
#[error("Unit not allowed here")]
pub struct NoUnitHere {
    #[source_code]
    pub src: NamedSource,
    #[label("Here")]
    pub span: SourceSpan,
}

impl NoUnitHere {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Can not create impure closure in pure function")]
pub struct InpureClosureInPureFunction {
    #[source_code]
    pub src: NamedSource,
    #[label("Closure Here")]
    pub span: SourceSpan,
}

impl InpureClosureInPureFunction {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Can not apply impure function (routine) in pure function")]
pub struct InpureFunctionInPureFunction {
    #[source_code]
    pub src: NamedSource,
    #[label("Application Here")]
    pub span: SourceSpan,
}

impl InpureFunctionInPureFunction {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Parse error")]
pub struct ParseError {
    #[source_code]
    pub src: NamedSource,
    #[label("Here {}", context)]
    pub span: SourceSpan,
    pub context: String,
}

impl ParseError {
    pub fn new(e: nom::error::VerboseError<&str>, src: NamedSource, src_len: usize) -> Self {
        let mut offset = 0;
        let mut context = String::from("");

        for (input, layer) in e.errors {
            if let nom::error::VerboseErrorKind::Context(c) = layer {
                context = format!("when trying to parse {}", c);
                break;
            }
            offset = src_len - input.len();
        }
        Self {
            src,
            span: (offset, 1).into(),
            context,
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("This is not a char")]
#[diagnostic()]
pub struct NotAChar {
    #[source_code]
    pub src: NamedSource,
    #[label("Char literal expression here")]
    pub span: SourceSpan,
}

impl NotAChar {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("hint")]
#[diagnostic()]
pub struct SpannedHint {
    pub msg: String,
    #[source_code]
    pub src: NamedSource,
    #[label("{msg}")]
    pub span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error)]
#[error("Details")]
#[diagnostic()]
pub struct TwoSpannedHint {
    #[related]
    hints: [SpannedHint; 2],
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unification Failure: can't unify types a = {a} with b = {b}")]
pub struct UnificationFailure {
    pub a: String,
    pub b: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Unification in this expression")]
    pub span: SourceSpan,
    #[source]
    #[diagnostic_source]
    pub hints: TwoSpannedHint,
}

impl UnificationFailure {
    pub fn new<'s>(
        a: &ast::Type<'s>,
        b: &ast::Type<'s>,
        meta: &ast::Meta,
        b_is_upper_bound: bool,
    ) -> Self {
        let b_repr = if b_is_upper_bound {
            format!("upper-bounded by {b}")
        } else {
            b.to_string()
        };
        UnificationFailure {
            a: a.to_string(),
            b: b_repr.clone(),
            src: meta.named_source(),
            span: meta.span(),
            hints: TwoSpannedHint {
                hints: [
                    SpannedHint {
                        msg: format!("Type a = {} here", a),
                        src: a.meta.named_source(),
                        span: a.meta.span.into(),
                    },
                    SpannedHint {
                        msg: format!("Type b = {} here", b_repr),
                        src: b.meta.named_source(),
                        span: b.meta.span.into(),
                    },
                ],
            },
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Value of type {} can not be callable", type_)]
#[diagnostic()]
pub struct NotCallable {
    pub type_: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Callee here")]
    pub span: SourceSpan,
}

impl NotCallable {
    pub fn new(type_: &ast::Type<'_>) -> Self {
        Self {
            type_: type_.node.to_string(),
            src: type_.meta.named_source(),
            span: type_.meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Test of an if-else expression must be bool")]
pub struct TestNotBool {
    #[source_code]
    pub src: NamedSource,
    #[label("Test here")]
    pub span: SourceSpan,
}

impl TestNotBool {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Global {} not found", name)]
#[diagnostic()]
pub struct GlobalNotFound {
    pub name: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Referenced here")]
    pub span: SourceSpan,
}

impl GlobalNotFound {
    pub fn new(name: &str, meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            name: name.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error(
    "Circular reference met when infering type of function {}",
    referenced_name
)]
#[diagnostic(help("Consider annotating one of the functions"))]
pub struct CircularInference {
    pub referenced_name: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Referenced function {} here", referenced_name)]
    pub span: SourceSpan,
}

impl CircularInference {
    pub fn new(ref_: &str, meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            referenced_name: ref_.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Constructor {} not found", name)]
pub struct ConstructorNotFound {
    name: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Referenced here")]
    pub span: SourceSpan,
}

impl ConstructorNotFound {
    pub fn new(name: &str, meta: &ast::Meta) -> Self {
        Self {
            name: name.to_string(),
            span: meta.span(),
            src: meta.named_source(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Wrong number of arguments")]
pub struct WrongNumberOfArgs {
    pub expected: usize,
    #[source_code]
    pub src: NamedSource,
    #[label("In type here")]
    pub span: SourceSpan,
}

impl WrongNumberOfArgs {
    pub fn new(expected: usize, meta: &ast::Meta) -> Self {
        Self {
            expected,
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error(
    "Precedence of infix operator {} not declared, continue parsing with default value 0",
    op
)]
pub struct UndeclaredInfixOperator {
    op: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Operator referenced here")]
    pub span: SourceSpan,
}

impl UndeclaredInfixOperator {
    pub fn new(meta: &ast::Meta, name: &str) -> Self {
        Self {
            op: name.to_string(),
            span: meta.span(),
            src: meta.named_source(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Tuple index out of bound")]
pub struct TupleIndexOutOfBound {
    #[source_code]
    pub src: NamedSource,
    #[label("Tuple here has type {}", type_)]
    pub span: SourceSpan,
    pub type_: String,
}

impl TupleIndexOutOfBound {
    pub fn new(tuple_type: &ast::Type) -> Self {
        Self {
            src: tuple_type.meta.named_source(),
            span: tuple_type.meta.span(),
            type_: tuple_type.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Type Annotation required for tuple")]
pub struct TypeAnnotationRequiredForTuple {
    #[source_code]
    pub src: NamedSource,
    #[label("Tuple-access operation on a variable here, type of which hasn't been determied")]
    pub span: SourceSpan,
}

impl TypeAnnotationRequiredForTuple {
    pub fn new(tuple_meta: &ast::Meta) -> Self {
        Self {
            src: tuple_meta.named_source(),
            span: tuple_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Can not do tuple-access operation on type {}", type_)]
pub struct NoTupleAccess {
    #[source_code]
    pub src: NamedSource,
    #[label("Tuple access here")]
    pub span: SourceSpan,
    pub type_: String,
}

impl NoTupleAccess {
    pub fn new(type_: &ast::Type) -> Self {
        Self {
            src: type_.meta.named_source(),
            span: type_.meta.span(),
            type_: type_.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Infinite type not supported")]
pub struct InfiniteType {
    #[source_code]
    pub src: NamedSource,
    #[label("Type variable {} here equals to {}", var_name, var_value)]
    pub span: SourceSpan,
    pub var_name: String,
    pub var_value: String,
}

impl InfiniteType {
    pub fn new(var_id: ast::TypeVarId, var_meta: &ast::Meta, var_value: &ast::Type) -> Self {
        Self {
            src: var_meta.named_source(),
            span: var_meta.span(),
            var_name: format!("'{}", var_id.0),
            var_value: var_value.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Constructor name must be uppercase")]
pub struct ConstructorNameNotUppercase {
    #[source_code]
    pub src: NamedSource,
    #[label("Constructor here")]
    pub span: SourceSpan,
}

impl ConstructorNameNotUppercase {
    pub fn new(constructor_meta: &ast::Meta) -> Self {
        Self {
            src: constructor_meta.named_source(),
            span: constructor_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Refutable pattern not allowed here")]
pub struct RefutablePattern {
    #[source_code]
    pub src: NamedSource,
    #[label("Pattern here")]
    pub span: SourceSpan,
}

impl RefutablePattern {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Too many arguments")]
pub struct TooManyArguments {
    expected: usize,
    #[source_code]
    pub src: NamedSource,
    #[label("Function call here expect at most {} arguments", expected)]
    pub span: SourceSpan,
}

impl TooManyArguments {
    pub fn new(meta: &ast::Meta, expected: usize) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            expected,
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Too many parameters. A maximum of 256 is supported.")]
pub struct TooManyParameters {
    given: usize,
    #[source_code]
    pub src: NamedSource,
    #[label("Function defined here has {} parameters", given)]
    pub span: SourceSpan,
}

impl TooManyParameters {
    pub fn new(meta: &ast::Meta, given: usize) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            given,
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Too many variants. A maximum of 256 is supported.")]
pub struct TooManyVariants {
    given: usize,
    #[source_code]
    pub src: NamedSource,
    #[label("Data defined here has {} variants", given)]
    pub span: SourceSpan,
}

impl TooManyVariants {
    pub fn new(meta: &ast::Meta, given: usize) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            given,
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Compile error")]
pub struct ManyError {
    #[related]
    e: Vec<miette::Report>,
}

impl ManyError {
    pub fn new() -> Self {
        ManyError { e: vec![] }
    }

    pub fn push(&mut self, e: impl Diagnostic + 'static + Send + Sync) {
        self.e.push(Report::new(e))
    }

    pub fn push_boxed(&mut self, e: Report) {
        self.e.push(e)
    }

    pub fn receive(&mut self, e: Result<(), impl Diagnostic + 'static + Send + Sync>) {
        e.map_or_else(|e| self.push(e), |x| x)
    }

    pub fn unwrap(&self) {
        if self.e.len() != 0 {
            for e in &self.e {
                eprintln!("{:?}", e)
            }
            panic!("Called ManyError::unwrap on some errors");
        }
    }

    pub fn emit(self) -> Result<(), miette::Report> {
        if self.e.len() == 0 {
            return Ok(());
        }
        Err(miette::Report::new(self))
    }
}

pub trait ManyErrorReceive {
    fn commit(self, e: &mut ManyError);
}

impl<E> ManyErrorReceive for Result<(), E>
where
    E: Diagnostic + 'static + Send + Sync,
{
    fn commit(self, e: &mut ManyError) {
        e.receive(self)
    }
}
