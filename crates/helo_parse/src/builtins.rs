use crate::ast;
use crate::errors;
use crate::parse;
use parse::tast;

use std::sync::Arc;

const BUILTIN_SIGS: &'static str = r#"
    module arith.int
        builtin fn +   a,b : [Int, Int] -> Int = ..
        builtin fn -   a,b : [Int, Int] -> Int = ..
        builtin fn *   a,b : [Int, Int] -> Int = ..
        builtin fn **  a,b : [Int, Int] -> Int = ..
        builtin fn /   a,b : [Int, Int] -> Int = ..
        builtin fn mod a,b : [Int, Int] -> Int = ..

        builtin fn == a,b : [Int, Int] -> Bool = ..
        builtin fn /= a,b : [Int, Int] -> Bool = ..
        builtin fn >= a,b : [Int, Int] -> Bool = ..
        builtin fn <= a,b : [Int, Int] -> Bool = ..
        builtin fn >  a,b : [Int, Int] -> Bool = ..
        builtin fn <  a,b : [Int, Int] -> Bool = ..
    end

    module arith.float
        builtin fn neg  a   : [Float] -> Float        = ..
        builtin fn +.   a,b : [Float, Float] -> Float = ..
        builtin fn -.   a,b : [Float, Float] -> Float = ..
        builtin fn *.   a,b : [Float, Float] -> Float = ..
        builtin fn **.  a,b : [Float, Int] -> Float   = ..
        builtin fn **.. a,b : [Float, Float] -> Float = ..
        builtin fn /.   a,b : [Float, Float] -> Float = ..

        builtin fn =.   a,b : [Float, Float] -> Bool = ..
        builtin fn /=.  a,b : [Float, Float] -> Bool = ..
        builtin fn <=.  a,b : [Float, Float] -> Bool = ..
        builtin fn >=.  a,b : [Float, Float] -> Bool = ..
        builtin fn >.   a,b : [Float, Float] -> Bool = ..
        builtin fn <.   a,b : [Float, Float] -> Bool = ..

        builtin fn int_to_float a : [Int] -> Float   = ..
        builtin fn floor_float  a : [Float] -> Int   = ..
        builtin fn ceil_float   a : [Float] -> Int   = ..
        builtin fn round_float  a : [Float] -> Int   = ..
    end

    module fmt
        builtin fn int_to_str   a     : [Int] -> Str      = ..
        builtin fn float_to_str a     : [Float] -> Str    = ..
        builtin fn bool_to_str  a     : [Bool] -> Str     = ..
        builtin fn char_to_str  a     : [Char] -> Str     = ..
    end

    module arith.bool
        builtin fn and a,b : [Bool, Bool] -> Bool = ..
        builtin fn or  a,b : [Bool, Bool] -> Bool = ..
        builtin fn not a   : [Bool] -> Bool       = ..
    end

    module arith.char
        builtin fn char_eq      a,b   : [Char, Char] -> Bool = ..
    end

    module str
        builtin fn str_cat      a,b   : [Str, Str] -> Str  = ..
        builtin fn str_some     a     : [Str] -> Bool      = ..
        builtin fn str_len      a     : [Str] -> Int       = ..
        builtin fn str_eq       a,b   : [Str, Str] -> Bool = ..
        builtin fn str_head     a     : [Str] -> Char      = ..
        builtin fn str_tail     a     : [Str] -> Str       = ..
    end

    module io
        builtin routine  println msg   : [Str] -> ()         = ..
        builtin routine  print   msg   : [Str] -> ()         = ..
        builtin routine  readline      : [] -> Str           = ..
    end

    module panic
        builtin fn panic 'a     msg   : [Str] -> 'a          = ..
    end

    module option
        data Option['a] = Some 'a
                        | None
    end

    module result
        data Result['a, 'e] = Ok 'a
                            | Err 'e
    end

    module list
        data List['a] = Con 'a, List['a] 
                      | Nil
    end
"#;

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
