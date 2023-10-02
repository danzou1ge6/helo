use helo_parse::ast::Meta;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Diagnostic, Debug, Error)]
#[error("Some branch in this function requires jumping {} instructions, but we only support jump distance that can be represented by 3 bytes", distance)]
pub struct TooLongJump {
    #[source_code]
    pub src: NamedSource,
    #[label("Function here")]
    pub span: SourceSpan,
    pub distance: usize,
}

impl TooLongJump {
    pub fn new(meta: &Meta, distance: usize) -> Self {
        Self {
            src: meta.named_source(),
            span: meta.span(),
            distance,
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Too long code. We only support 4-byte wide pointer, so code cant take more than 4GB. Currently code length is  {}", current_len)]
pub struct TooLongCode {
    pub current_len: usize,
}

#[derive(Diagnostic, Debug, Error)]
#[error("No function `main` found, which is the entry point of the program.")]
pub struct MainNotFound {}
