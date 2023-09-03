use crate::parse;
use crate::{ast, errors};

use std::sync::Arc;

const BUILTIN_SIGS: &'static str = r#"
    let fn + a,b : [Int, Int] -> Int = ..
    let fn - a,b : [Int, Int] -> Int = ..
    let fn * a,b : [Int, Int] -> Int = ..
    let fn / a,b : [Int, Int] -> Int = ..

    let fn == a,b : [Int, Int] -> Bool = ..
    let fn /= a,b : [Int, Int] -> Bool = ..
    let fn >= a,b : [Int, Int] -> Bool = ..
    let fn <= a,b : [Int, Int] -> Bool = ..
    let fn >  a,b : [Int, Int] -> Bool = ..
    let fn <  a,b : [Int, Int] -> Bool = ..

    let fn int_to_float a : [Int] -> Float  = ..
    let fn floot_float  a : [Float] -> Int  = ..

    let fn +. a,b : [Float, Float] -> Float = ..
    let fn -. a,b : [Float, Float] -> Float = ..
    let fn *. a,b : [Float, Float] -> Float = ..
    let fn /. a,b : [Float, Float] -> Float = ..

    let fn and a,b : [Bool, Bool] -> Bool = ..
    let fn or  a,b : [Bool, Bool] -> Bool = ..
    let fn not a   : [Bool] -> Bool       = ..

    let fn str_cat      a,b   : [Str, Str] -> Str = ..
    let fn int_to_str   a     : [Int] -> Str      = ..
    let fn bool_to_str  a     : [Bool] -> Str     = ..
    let fn float_to_str a     : [Float] -> Str    = ..
"#;

const BUILTIN_DEF: &'static str = r#"
    infix `and` 41 40
    infix `or` 41 40

    infix + 41 40
    infix - 41 40
    infix * 51 50
    infix / 51 50

    infix == 31 30
    infix /= 31 30
    infix >= 31 30
    infix <= 31 30
    infix >  31 30
    infix <  31 30

    infix +. 41 40
    infix -. 41 40
    infix *. 51 50
    infix /. 51 50

    infix `str_cat` 41 40

    data Option['a] = Some 'a
                    | None

    data Result['a, 'e] = Ok 'a
                        | Err 'e

    data List['a] = Con 'a, List['a] 
                  | Nil

    data Pair['a, 'b] = Pair 'a, 'b

"#;

pub fn add_builtins_to<'s>(
    symbols: &mut ast::Symbols<'s>,
    ast_nodes: &mut ast::ExprHeap<'s>,
    precedence_table: &mut parse::PrecedenceTable<'s>,
) {
    let sigs_src = Arc::new(BUILTIN_SIGS.to_owned());
    let file_name = Arc::new("<builtins>".to_owned());
    let defs_src = Arc::new(BUILTIN_DEF.to_owned());

    let mut e = errors::ManyError::new();
    parse::parse_ast(
        BUILTIN_DEF,
        defs_src,
        file_name.clone(),
        symbols,
        ast_nodes,
        &mut e,
        precedence_table,
    )
    .unwrap();
    e.unwrap();

    parse::parse_builtin_function_signatures(BUILTIN_SIGS, sigs_src, file_name, symbols);
}
