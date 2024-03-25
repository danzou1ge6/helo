use crate::ast;
use crate::errors;
use crate::parse;
use parse::tast;

use std::sync::Arc;

const BUILTIN_SIGS: &'static str = include_str!("./builtin_defs.helo");

pub fn add_builtins_to<'s>(symbols: &mut tast::Symbols<'s>) -> Result<(), errors::ParseError> {
    let sigs_src = Arc::new(BUILTIN_SIGS.to_owned());
    let file_name = Arc::new("<builtins>".to_owned());

    parse::parse_ast(
        BUILTIN_SIGS,
        sigs_src,
        file_name,
        ast::Path::new([]),
        symbols,
    )
}
