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

        instance ops.Add Int,Int,Int
            fn + a,b = a + b
        end
        instance ops.Subs Int,Int,Int
            fn - a,b = a - b
        end
        instance ops.Mul Int,Int,Int
            fn * a,b = a * b
        end
        instance ops.Div Int,Int,Int
            fn / a,b = a / b
        end
        instance ops.Pow Int,Int,Int
            fn ** a,b = a ** b
        end
        instance ops.Mod Int,Int,Int
            fn mod a,b = mod a,b
        end

        builtin fn == a,b : [Int, Int] -> Bool = ..
        builtin fn /= a,b : [Int, Int] -> Bool = ..
        builtin fn >= a,b : [Int, Int] -> Bool = ..
        builtin fn <= a,b : [Int, Int] -> Bool = ..
        builtin fn >  a,b : [Int, Int] -> Bool = ..
        builtin fn <  a,b : [Int, Int] -> Bool = ..

        instance ops.Eq Int,Int
            fn == a,b = a == b
        end
        instance ops.Ne Int,Int
            fn /= a,b = a /= b
        end
        instance ops.Gt Int,Int
            fn > a,b = a > b
        end
        instance ops.Lt Int,Int
            fn < a,b = a < b
        end
        instance ops.Ge Int,Int
            fn >= a,b = a >= b
        end
        instance ops.Le Int,Int
            fn <= a,b = a <= b
        end

    end

    module arith.float
        builtin fn neg  a   : [Float] -> Float        = ..
        builtin fn +.   a,b : [Float, Float] -> Float = ..
        builtin fn -.   a,b : [Float, Float] -> Float = ..
        builtin fn *.   a,b : [Float, Float] -> Float = ..
        builtin fn **.  a,b : [Float, Int] -> Float   = ..
        builtin fn **.. a,b : [Float, Float] -> Float = ..
        builtin fn /.   a,b : [Float, Float] -> Float = ..

        instance ops.Add Float,Float,Float
            fn + a,b = a +. b
        end
        instance ops.Subs Float,Float,Float
            fn - a,b = a -. b
        end
        instance ops.Mul Float,Float,Float
            fn * a,b = a *. b
        end
        instance ops.Div Float,Float,Float
            fn / a,b = a /. b
        end
        instance ops.Pow Float,Float,Float
            fn ** a,b = a **.. b
        end
        instance ops.Pow Float,Int,Float
            fn ** a,b = a **. b
        end

        builtin fn =.   a,b : [Float, Float] -> Bool = ..
        builtin fn /=.  a,b : [Float, Float] -> Bool = ..
        builtin fn <=.  a,b : [Float, Float] -> Bool = ..
        builtin fn >=.  a,b : [Float, Float] -> Bool = ..
        builtin fn >.   a,b : [Float, Float] -> Bool = ..
        builtin fn <.   a,b : [Float, Float] -> Bool = ..

        instance ops.Eq Float,Float
            fn == a,b = a =. b
        end
        instance ops.Ne Float,Float
            fn /= a,b = a /=. b
        end
        instance ops.Gt Float,Float
            fn > a,b = a >. b
        end
        instance ops.Lt Float,Float
            fn < a,b = a <. b
        end
        instance ops.Ge Float,Float
            fn >= a,b = a >=. b
        end
        instance ops.Le Float,Float
            fn <= a,b = a <=. b
        end

        builtin fn int_to_float a : [Int] -> Float   = ..
        builtin fn floor_float  a : [Float] -> Int   = ..
        builtin fn ceil_float   a : [Float] -> Int   = ..
        builtin fn round_float  a : [Float] -> Int   = ..

        instance convert.From Float,Int
            fn from i = int_to_float i
        end

    end

    module fmt
        builtin fn int_to_str   a     : [Int] -> Str      = ..
        builtin fn float_to_str a     : [Float] -> Str    = ..
        builtin fn bool_to_str  a     : [Bool] -> Str     = ..
        builtin fn char_to_str  a     : [Char] -> Str     = ..

        relation ToString 'a
            fn to_string x : ['a] -> Str
        end

        instance ToString Int
            fn to_string i = int_to_str i
        end
        instance ToString Float
            fn to_string f = float_to_str f
        end
        instance ToString Bool
            fn to_string b = bool_to_str b
        end
        instance ToString Char
            fn to_string c = char_to_str c
        end
    end

    module arith.bool
        builtin fn and a,b : [Bool, Bool] -> Bool = ..
        builtin fn or  a,b : [Bool, Bool] -> Bool = ..
        builtin fn not a   : [Bool] -> Bool       = ..
    end

    module arith.char
        builtin fn char_eq      a,b   : [Char, Char] -> Bool = ..
        
        instance ops.Eq Char,Char
            fn == a,b = char_eq a,b
        end
    end

    module str
        builtin fn str_cat      a,b   : [Str, Str] -> Str  = ..
        builtin fn str_some     a     : [Str] -> Bool      = ..
        builtin fn str_len      a     : [Str] -> Int       = ..
        builtin fn str_eq       a,b   : [Str, Str] -> Bool = ..
        builtin fn str_head     a     : [Str] -> Char      = ..
        builtin fn str_tail     a     : [Str] -> Str       = ..

        instance ops.Mul Str,Str,Str
            fn * a,b = str_cat a,b
        end
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

    module ops
        relation Add 'a,'b,'c -> 'c
            fn + a,b : ['a, 'b] -> 'c
        end
        relation Subs 'a,'b,'c -> 'c
            fn - a,b : ['a, 'b] -> 'c
        end
        relation Mul 'a,'b,'c -> 'c
            fn * a,b : ['a, 'b] -> 'c
        end
        relation Div 'a,'b,'c -> 'c
            fn / a,b : ['a, 'b] -> 'c
        end
        relation Pow 'a,'b,'c -> 'c
            fn ** a,b : ['a, 'b] -> 'c
        end
        relation Mod 'a,'b,'c -> 'c
            fn mod a,b : ['a, 'b] -> 'c
        end

        relation Eq 'a,'b
            fn == a,b : ['a, 'b] -> Bool
        end
        relation Ne 'a,'b
            fn /= a,b : ['a, 'b] -> Bool
        end
        relation Lt 'a,'b
            fn < a,b : ['a, 'b] -> Bool
        end
        relation Gt 'a,'b
            fn > a,b : ['a, 'b] -> Bool
        end
        relation Le 'a,'b
            fn <= a,b : ['a, 'b] -> Bool
        end
        relation Ge 'a,'b
            fn >= a,b : ['a, 'b] -> Bool
        end
    end

    module convert
        relation From 'a,'b
            fn from b : ['b] -> 'a
        end
        relation Into 'a,'b
            fn into a : ['a] -> 'b
        end
        instance 'a,'b Into 'a,'b where From 'b,'a
            fn into a = from a
        end
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
