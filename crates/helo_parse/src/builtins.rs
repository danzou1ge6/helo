use crate::ast;
use crate::parse;

use std::sync::Arc;

const BUILTIN_SIGS: &'static str = r#"
    fn +   a,b : [Int, Int] -> Int = ..
    fn -   a,b : [Int, Int] -> Int = ..
    fn *   a,b : [Int, Int] -> Int = ..
    fn **  a,b : [Int, Int] -> Int = ..
    fn /   a,b : [Int, Int] -> Int = ..
    fn mod a,b : [Int, Int] -> Int = ..

    fn == a,b : [Int, Int] -> Bool = ..
    fn /= a,b : [Int, Int] -> Bool = ..
    fn >= a,b : [Int, Int] -> Bool = ..
    fn <= a,b : [Int, Int] -> Bool = ..
    fn >  a,b : [Int, Int] -> Bool = ..
    fn <  a,b : [Int, Int] -> Bool = ..

    fn neg  a   : [Float] -> Float        = ..
    fn +.   a,b : [Float, Float] -> Float = ..
    fn -.   a,b : [Float, Float] -> Float = ..
    fn *.   a,b : [Float, Float] -> Float = ..
    fn **.  a,b : [Float, Int] -> Float   = ..
    fn **.. a,b : [Float, Float] -> Float = ..
    fn /.   a,b : [Float, Float] -> Float = ..

    fn =.   a,b : [Float, Float] -> Bool = ..
    fn /=.  a,b : [Float, Float] -> Bool = ..
    fn <=.  a,b : [Float, Float] -> Bool = ..
    fn >=.  a,b : [Float, Float] -> Bool = ..
    fn >.   a,b : [Float, Float] -> Bool = ..
    fn <.   a,b : [Float, Float] -> Bool = ..

    fn int_to_float a : [Int] -> Float   = ..
    fn floor_float  a : [Float] -> Int   = ..
    fn ceil_float   a : [Float] -> Int   = ..
    fn round_float  a : [Float] -> Int   = ..

    fn int_to_str   a     : [Int] -> Str      = ..
    fn float_to_str a     : [Float] -> Str    = ..
    fn bool_to_str  a     : [Bool] -> Str     = ..
    fn char_to_str  a     : [Char] -> Str     = ..

    fn and a,b : [Bool, Bool] -> Bool = ..
    fn or  a,b : [Bool, Bool] -> Bool = ..
    fn not a   : [Bool] -> Bool       = ..

    fn char_eq      a,b   : [Char, Char] -> Bool = ..

    fn str_cat      a,b   : [Str, Str] -> Str  = ..
    fn str_some     a     : [Str] -> Bool      = ..
    fn str_len      a     : [Str] -> Int       = ..
    fn str_eq       a,b   : [Str, Str] -> Bool = ..
    fn str_head     a     : [Str] -> Char      = ..
    fn str_tail     a     : [Str] -> Str       = ..

    fn panic 'a     msg   : [Str] -> Char        = ..

    routine  println msg   : [Str] -> ()         = ..
"#;

const PRECEDENCE_DEF: &'static str = r#"
    infix `and` 41 40
    infix `or`  41 40

    infix +     41 40
    infix -     41 40
    infix *     51 50
    infix **    61 60
    infix /     51 50
    infix `mod` 51 50

    infix == 31 30
    infix /= 31 30
    infix >= 31 30
    infix <= 31 30
    infix >  31 30
    infix <  31 30

    infix +.   41 40
    infix -.   41 40
    infix *.   51 50
    infix **.  61 60
    infix **.. 61 60
    infix /.   51 50

    infix `str_cat` 41 40
    infix `str_eq` 31 30
    infix `char_eq` 31 30
"#;

const DATA_DEF: &'static str = r#"
    data Option['a] = Some 'a
                    | None

    data Result['a, 'e] = Ok 'a
                        | Err 'e

    data List['a] = Con 'a, List['a] 
                  | Nil

"#;

pub fn add_builtins_to<'s>(
    symbols: &mut ast::Symbols<'s>,
    precedence_table: &mut parse::PrecedenceTable<'s>,
) {
    let sigs_src = Arc::new(BUILTIN_SIGS.to_owned());
    let data_src = Arc::new(DATA_DEF.to_string());
    let file_name = Arc::new("<builtins>".to_owned());
    parse::parse_infix_declarations(PRECEDENCE_DEF, precedence_table);
    parse::parse_builtin_function_signatures(BUILTIN_SIGS, sigs_src, file_name.clone(), symbols);
    parse::parse_builtin_data(DATA_DEF, data_src, file_name, symbols);
}
