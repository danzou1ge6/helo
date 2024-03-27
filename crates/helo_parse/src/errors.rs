use std::sync::Arc;

use crate::inferer::Inferer;
use crate::{ast, parse::tast};
use miette::{Diagnostic, Report, SourceSpan};
use thiserror::Error;

type NamedSource = miette::NamedSource<Arc<String>>;

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
#[error("Type variable ubound")]
#[diagnostic(help("Consider annotating the type of the function containing this expression"))]
pub struct UnboundTypeVariable {
    #[source_code]
    pub src: NamedSource,
    #[label("Type for expression here")]
    pub span: SourceSpan,
}

impl UnboundTypeVariable {
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
#[error("Unification Failure: can't unify types a = {a} with b = {b}")]
pub struct UnificationFailure {
    pub a: String,
    pub b: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Unification in this expression")]
    pub span: SourceSpan,
    #[label("a here")]
    pub a_span: SourceSpan,
    #[label("b here")]
    pub b_span: SourceSpan,
}

impl UnificationFailure {
    pub fn new<'s>(
        a: &ast::Type<'s>,
        a_meta: &ast::Meta,
        b: &ast::Type<'s>,
        b_meta: &ast::Meta,
        meta: &ast::Meta,
        inferer: &Inferer<'s>,
    ) -> Self {
        UnificationFailure {
            a: type_to_string(a, inferer, a_meta),
            b: type_to_string(b, inferer, b_meta),
            src: meta.named_source(),
            span: meta.span(),
            a_span: a_meta.span(),
            b_span: b_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unification Failure: can't unify local {local} with value of type {value}")]
pub struct LocalUnificationFailure {
    pub local: String,
    pub value: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Unification in this expression")]
    pub span: SourceSpan,
    #[label("Value here")]
    pub value_span: SourceSpan,
}

impl LocalUnificationFailure {
    pub fn new<'s>(
        local: ast::LocalId,
        value: &ast::Type<'s>,
        value_meta: &ast::Meta,
        meta: &ast::Meta,
        inferer: &Inferer<'s>,
    ) -> Self {
        Self {
            local: local.to_string(),
            value: type_to_string(value, inferer, meta),
            src: meta.named_source(),
            span: meta.span(),
            value_span: value_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unification Failure: can't unify captured local with value of type {value}")]
pub struct CaptureUnificationFailure {
    pub value: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Variable here is captured")]
    pub span: SourceSpan,
}

impl CaptureUnificationFailure {
    pub fn new<'s>(value: &ast::Type<'s>, meta: &ast::Meta, inferer: &Inferer<'s>) -> Self {
        Self {
            value: type_to_string(value, inferer, meta),
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unification Failure: Test-expr must be type Bool")]
pub struct NonBoolTest {
    pub value: String,
    #[source_code]
    pub src: NamedSource,
    #[label("In expression here")]
    pub span: SourceSpan,
    #[label("Test-expr here has type {}", .value)]
    pub value_span: SourceSpan,
}

impl NonBoolTest {
    pub fn new<'s>(value: &ast::Type<'s>, value_meta: &ast::Meta, meta: &ast::Meta) -> Self {
        Self {
            value: value.to_string(),
            src: meta.named_source(),
            span: meta.span(),
            value_span: value_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unification Failure: Type of function body does not match provided annotation")]
pub struct BodyTypeMismatchAnnotation {
    pub value: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Function body here has type {}", .value)]
    pub span: SourceSpan,
}

impl BodyTypeMismatchAnnotation {
    pub fn new<'s>(value: &ast::Type<'s>, meta: &ast::Meta, inferer: &Inferer<'s>) -> Self {
        Self {
            value: type_to_string(value, inferer, meta),
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unification Failure: Can not infer type of pattern-expression")]
pub struct PatternUnificationFailure {
    pub arg_type: String,
    pub param_type: String,
    pub i: usize,
    #[source_code]
    pub src: NamedSource,
    #[label("In expression here")]
    pub span: SourceSpan,
    #[label("Argument to constructor here has type {}, but the {}th parameter of the constructor has type {}", arg_type, i, param_type)]
    pub arg_span: SourceSpan,
}

impl PatternUnificationFailure {
    pub fn new<'s>(
        arg_type: &ast::Type<'s>,
        param_type: &ast::Type<'s>,
        i: usize,
        arg_meta: &ast::Meta,
        ctx_meta: &ast::Meta,
        inferer: &Inferer<'s>,
    ) -> Self {
        Self {
            arg_type: type_to_string(arg_type, inferer, arg_meta),
            param_type: type_to_string(param_type, inferer, ctx_meta),
            i,
            src: ctx_meta.named_source(),
            span: ctx_meta.span(),
            arg_span: arg_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unification Failure: Case-arm has different type from previous arms")]
pub struct ArmTypeUnificationFailure {
    pub value: String,
    #[source_code]
    pub src: NamedSource,
    #[label("In expression here")]
    pub span: SourceSpan,
    #[label("Case-arm here has type {}", .value)]
    pub value_span: SourceSpan,
}

impl ArmTypeUnificationFailure {
    pub fn new<'s>(
        value: &ast::Type<'s>,
        value_meta: &ast::Meta,
        meta: &ast::Meta,
        inferer: &Inferer<'s>,
    ) -> Self {
        Self {
            value: type_to_string(value, inferer, value_meta),
            src: meta.named_source(),
            span: meta.span(),
            value_span: value_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unification Failure: can't unify callable type a = {a} with argument type b = {b}")]
pub struct ArgumentUnificationFailure {
    pub a: String,
    pub b: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Argument here")]
    pub arg_span: SourceSpan,
    #[label("Callee here")]
    pub callee_span: SourceSpan,
}

fn type_to_string<'s>(t: &ast::Type<'s>, inferer: &Inferer<'s>, meta: &ast::Meta) -> String {
    inferer
        .resolve(t, meta)
        .map_or_else(|_| "<Infinite Type>".to_string(), |t| t.to_string())
}

impl ArgumentUnificationFailure {
    pub fn new<'s>(
        a: &ast::Type<'s>,
        callee_meta: &ast::Meta,
        b: &ast::Type<'s>,
        arg_meta: &ast::Meta,
        meta: &ast::Meta,
        inferer: &Inferer<'s>,
    ) -> Self {
        Self {
            a: type_to_string(a, inferer, callee_meta),
            b: type_to_string(b, inferer, arg_meta),
            src: meta.named_source(),
            callee_span: callee_meta.span(),
            arg_span: arg_meta.span(),
        }
    }
}

#[derive(Debug, Diagnostic, Error)]
#[error("Unification Failure: can't unify callable type {callee} with arguments of types {args}")]
pub struct ArgumentsUnificationFailure {
    pub callee: String,
    pub args: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Unification in this expression")]
    pub span: SourceSpan,
}

impl ArgumentsUnificationFailure {
    pub fn new<'s: 'a, 'a>(
        callee_type: &ast::Type<'s>,
        mut args_types: impl Iterator<Item = &'a ast::Type<'s>>,
        meta: &ast::Meta,
        inferer: &Inferer<'s>,
    ) -> Self {
        Self {
            callee: callee_type.to_string(),
            args: {
                let mut s = String::new();
                if let Some(first) = args_types.next() {
                    s.push_str(&type_to_string(&first, inferer, meta));
                }
                for arg_typ in args_types {
                    s.push_str(&format!(", {}", arg_typ));
                }
                s
            },
            src: meta.named_source(),
            span: meta.span(),
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
    pub fn new(type_: &ast::Type<'_>, type_meta: &ast::Meta) -> Self {
        Self {
            type_: type_.node.to_string(),
            src: type_meta.named_source(),
            span: type_meta.span(),
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
    pub fn new(ref_: ast::FunctionId, meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            referenced_name: format!("{:?}", ref_),
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
    pub fn new(name: ast::ConstructorName<'_>, meta: &ast::Meta) -> Self {
        Self {
            name: format!("{:?}", name),
            span: meta.span(),
            src: meta.named_source(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Data {} not found", name)]
pub struct DataNotFound {
    name: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Referenced here")]
    pub span: SourceSpan,
}

impl DataNotFound {
    pub fn new(name: ast::DataName<'_>, meta: &ast::Meta) -> Self {
        Self {
            name: format!("{:?}", name),
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
    pub fn new(tuple_type: &ast::Type, meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            type_: tuple_type.to_string(),
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

#[derive(Diagnostic, Debug, Error)]
#[error("Constrain not satisfied")]
pub struct ConstrainProofFailed {
    constrain: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Relation-method here require constrain {}", constrain)]
    pub span: SourceSpan,
}

impl ConstrainProofFailed {
    pub fn new(meta: &ast::Meta, c: &ast::Constrain<'_>) -> Self {
        Self {
            constrain: c.to_string(),
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Too many hit of matching instance")]
pub struct TooManyHitMatchingInstance {
    #[related]
    pub instances: Vec<SpannedHint>,
    pub constrain: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Relation-method here require constrain {}", constrain)]
    pub span: SourceSpan,
}

impl TooManyHitMatchingInstance {
    pub fn new(instance_metas: impl Iterator<Item = ast::Meta>, c: &ast::Constrain) -> Self {
        let instances = instance_metas
            .map(|m| SpannedHint {
                msg: "Instance here is a match".to_string(),
                src: m.named_source(),
                span: m.span(),
            })
            .collect();
        Self {
            instances,
            constrain: c.to_string(),
            src: c.meta.named_source(),
            span: c.meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Method not implemented")]
pub struct MethodNotImplemented {
    pub method_name: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Method `{}` required in this instance", method_name)]
    pub span: SourceSpan,
}

impl MethodNotImplemented {
    pub fn new(method_name: &str, ins_meta: &ast::Meta) -> Self {
        Self {
            method_name: method_name.to_string(),
            src: ins_meta.named_source(),
            span: ins_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Undeclared methond")]
pub struct UndeclaredMethod {
    #[source_code]
    pub src: NamedSource,
    #[label("This method is not declared in relation definition ")]
    pub span: SourceSpan,
}

impl UndeclaredMethod {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Method doesnot match signature in relation definition")]
pub struct MethodUnmatchRelationDefinition {
    pub method_sig: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Method here has signature {}", method_sig)]
    pub span: SourceSpan,
}

impl MethodUnmatchRelationDefinition {
    pub fn new(method_sig: &ast::MethodSig<'_>, method_meta: &ast::Meta) -> Self {
        Self {
            method_sig: method_sig.to_string(),
            src: method_meta.named_source(),
            span: method_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Relation constrains cannot be satisfied")]
pub struct RelationConstrainUnsatisfied {
    pub constrain: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Instance here need constrain `{}` to be satisfied", constrain)]
    pub span: SourceSpan,
}

impl RelationConstrainUnsatisfied {
    pub fn new(constrain: &ast::Constrain<'_>, ins_meta: &ast::Meta) -> Self {
        Self {
            constrain: constrain.to_string(),
            src: ins_meta.named_source(),
            span: ins_meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Relation doesnot exist")]
pub struct RelationNonExists {
    #[source_code]
    pub src: NamedSource,
    #[label("Relation mentioned here")]
    pub span: SourceSpan,
}

impl RelationNonExists {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Relation arity wrong")]
pub struct RelationArityWrong {
    pub expected: usize,
    #[source_code]
    pub src: NamedSource,
    #[label("Relation mentioned here")]
    pub span: SourceSpan,
}

impl RelationArityWrong {
    pub fn new(meta: &ast::Meta, expected: usize) -> Self {
        Self {
            expected,
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Method type annotation not supported")]
pub struct MethodTypeAnnotationNotSupported {
    #[source_code]
    pub src: NamedSource,
    #[label("Relation mentioned here")]
    pub span: SourceSpan,
}

impl MethodTypeAnnotationNotSupported {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Identifier path beyond root")]
pub struct ModulePathBeyondRoot {
    #[source_code]
    pub src: NamedSource,
    #[label("Identifier path `{}` here goes beyond root", path)]
    pub span: SourceSpan,
    pub path: String,
}

impl ModulePathBeyondRoot {
    pub fn new(meta: &ast::Meta, path: &tast::Path<'_>) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            path: path.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Bad access operands. Only identifiers are expected.")]
pub struct BadAccessOperands {
    #[source_code]
    pub src: NamedSource,
    #[label("Operands to dot operator here are incorrect")]
    pub span: SourceSpan,
}

impl BadAccessOperands {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Ambiguity in resolving identifier path")]
pub struct AmbiguityResolvingSymbol {
    #[source_code]
    pub src: NamedSource,
    #[label("Identifier path `{}` resolves to `{}`", path, hits)]
    pub span: SourceSpan,
    pub hits: String,
    pub path: String,
}

impl AmbiguityResolvingSymbol {
    pub fn new<'s>(meta: &ast::Meta, hits: &[ast::Path<'s>], path: &tast::Path<'s>) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            hits: ast::paths_to_string(hits),
            path: path.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Identifier path {} not found", name)]
#[diagnostic()]
pub struct IdentifierPathNotFound {
    pub name: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Referenced here")]
    pub span: SourceSpan,
}

impl IdentifierPathNotFound {
    pub fn new(name: &tast::Path<'_>, meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            name: name.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Function {} not found", name)]
pub struct FunctionNotFound {
    pub name: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Referenced here")]
    pub span: SourceSpan,
}

impl FunctionNotFound {
    pub fn new(name: &ast::FunctionName<'_>, meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            name: name.to_string(),
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Expect a constructor here, but not a binding, literal, application or tuple")]
pub struct ExpectedConstructor {
    #[source_code]
    pub src: NamedSource,
    #[label("In pattern application here")]
    pub span: SourceSpan,
}

impl ExpectedConstructor {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error(
    "Constructor `{}` does not exist, but neither does this look like a local variable binding",
    name
)]
pub struct NeitherConstructorNorBinding {
    pub name: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Referenced here")]
    pub span: SourceSpan,
}

impl NeitherConstructorNorBinding {
    pub fn new(name: &tast::Path<'_>, meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            name: name.to_string(),
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Expect a local variable binding here, but got a path")]
pub struct ExpectBindingGotPath {
    #[source_code]
    pub src: NamedSource,
    #[label("Referenced here")]
    pub span: SourceSpan,
}

impl ExpectBindingGotPath {
    pub fn new(meta: &ast::Meta) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Dependent type variable {} not found", var)]
pub struct DependentTypeVariableNotFound {
    pub var: String,
    #[source_code]
    pub src: NamedSource,
    #[label("Referenced in this relation")]
    pub span: SourceSpan
}

impl DependentTypeVariableNotFound {
    pub fn new(meta: &ast::Meta, var: &str) -> Self {
        Self {
            var: var.to_string(),
            src: meta.named_source(),
            span: meta.span(),
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

    pub fn emit(self) -> Result<Self, miette::Report> {
        if self.e.len() == 0 {
            return Ok(self);
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
