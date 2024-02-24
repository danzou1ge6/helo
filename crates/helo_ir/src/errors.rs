use helo_parse::ast::{self, Meta};

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Diagnostic, Debug, Error)]
#[error("Some branch in this function requires jumping from instruction {} to {}, but we only support jump distance that can be represented by an i16", from, to)]
pub struct TooLongJump {
    #[source_code]
    pub src: NamedSource,
    #[label("Function here")]
    pub span: SourceSpan,
    pub from: usize,
    pub to: usize,
}

impl TooLongJump {
    pub fn new(meta: &Meta, from: usize, to: usize) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            from,
            to,
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Too long code. We only support 4-byte wide pointer, so code cant take more than 4GB. Currently code length is  {}", current_len)]
pub struct TooLongCode {
    pub current_len: usize,
}

#[derive(Diagnostic, Debug, Error)]
#[error("Generic functions cannot be entry points")]
pub struct InvalidEntryPoint {
    #[source_code]
    pub src: NamedSource,
    #[label("Function here has type `{}`", type_)]
    pub span: SourceSpan,
    pub type_: String,
}

impl InvalidEntryPoint {
    pub fn new(f_meta: &Meta, type_: &ast::FunctionType<'_>) -> Self {
        Self {
            src: f_meta.named_source(),
            span: f_meta.span(),
            type_: type_.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("No function `main` found, which is the entry point of the program.")]
pub struct MainNotFound {}

#[derive(Diagnostic, Debug, Error)]
#[error("`main` must have type `^[] -> ()`")]
pub struct MainBadType {
    type_: String,
    #[source_code]
    pub src: NamedSource,
    #[label("But it has type `{}`", type_)]
    pub span: SourceSpan,
}

impl MainBadType {
    pub fn new(f_meta: &Meta, type_: &ast::FunctionType<'_>) -> Self {
        Self {
            src: f_meta.named_source(),
            span: f_meta.span(),
            type_: type_.to_string(),
        }
    }
}
