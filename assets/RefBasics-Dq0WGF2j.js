import{Q as g}from"./QCard-BErQhV2q.js";import{V as F}from"./PlayGround-CWbEsUBC.js";import{T as u,E as s}from"./TextHeader2-DlbFR_Q1.js";import{T as y,a as l}from"./TextBody1-Cc3uXhbj.js";import{T as a}from"./TextCode-BRaHiW80.js";import{d as x,r,a as w,f as B,b as e,w as t,F as v,e as n,E as _,G as S,_ as T}from"./index-mfOJ6gAK.js";import"./QScrollObserver-B-90Bz8d.js";import"./QBtn-CVSt3djh.js";const d=`module arith.int
    builtin fn +   a,b : [Int, Int] -> Int = ..
    builtin fn -   a,b : [Int, Int] -> Int = ..
    builtin fn *   a,b : [Int, Int] -> Int = ..
    builtin fn **  a,b : [Int, Int] -> Int = ..
    builtin fn /   a,b : [Int, Int] -> Int = ..
    builtin fn mod a,b : [Int, Int] -> Int = ..

    instance ops.Add Int,Int
        fn + a,b = a + b
    end
    instance ops.Subs Int,Int
        fn - a,b = a - b
    end
    instance ops.Mul Int,Int
        fn * a,b = a * b
    end
    instance ops.Div Int,Int
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

    instance ops.Eq Int
        fn == a,b = a == b
        fn /= a,b = a /= b
    end
    instance ops.Gt Int
        fn > a,b = a > b
    end
    instance ops.Lt Int
        fn < a,b = a < b
    end
    instance ops.Ge Int
        fn >= a,b = a >= b
    end
    instance ops.Le Int
        fn <= a,b = a <= b
    end
    instance ops.Ord Int
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

    instance ops.Add Float,Float
        fn + a,b = a +. b
    end
    instance ops.Subs Float,Float
        fn - a,b = a -. b
    end
    instance ops.Mul Float,Float
        fn * a,b = a *. b
    end
    instance ops.Div Float,Float
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

    instance ops.Eq Float
        fn == a,b = a =. b
        fn /= a,b = a /=. b
    end
    instance ops.Gt Float
        fn > a,b = a >. b
    end
    instance ops.Lt Float
        fn < a,b = a <. b
    end
    instance ops.Ge Float
        fn >= a,b = a >=. b
    end
    instance ops.Le Float
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
    builtin fn eq      a,b   : [Char, Char] -> Bool = ..
    
    instance ops.Eq Char
        fn == a,b = eq a,b
        fn /= a,b = arith.bool.not eq a,b
    end
end

module str
    builtin fn cat      a,b   : [Str, Str] -> Str  = ..
    builtin fn some     a     : [Str] -> Bool      = ..
    builtin fn len      a     : [Str] -> Int       = ..
    builtin fn eq       a,b   : [Str, Str] -> Bool = ..
    builtin fn head     a     : [Str] -> Char      = ..
    builtin fn tail     a     : [Str] -> Str       = ..

    instance ops.Mul Str,Str
        fn * a,b = cat a,b
    end
    instance ops.Eq Str
        fn == a,b = eq a,b
        fn /= a,b = arith.bool.not eq a,b
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
    
    fn unwrap x = case x of
        | Some x -> x
        | _ -> panic.panic "called unwrap on \`None\`"
    end
end

module result
    data Result['a, 'e] = Ok 'a
                        | Err 'e
end

module list
    data List['a] = :: 'a, List['a] 
                    | []
    
    fn head xs = case xs of
        | x::xs -> option.Some x
        | [] -> option.None
    end

    fn tail xs = case xs of
        | x::xs -> option.Some xs
        | [] -> option.None
    end

    fn is_empty xs = case xs of
        | [] -> true
        | _ -> false
    end

    fn rev 'a xs: [List['a]] -> List['a] =
        let fn helper as, bs: [List['a], List['a]] -> List['a] =
            case as of
            | [] -> bs
            | a :: as -> helper as, a :: bs
            end
        in
        helper xs,[]
end

module ops
    relation Add 'a,'c -> 'c
        fn + a,b : ['a, 'a] -> 'c
    end
    relation Subs 'a,'c -> 'c
        fn - a,b : ['a, 'a] -> 'c
    end
    relation Mul 'a,'c -> 'c
        fn * a,b : ['a, 'a] -> 'c
    end
    relation Div 'a,'c -> 'c
        fn / a,b : ['a, 'a] -> 'c
    end
    relation Pow 'a,'b,'c -> 'c
        fn ** a,b : ['a, 'b] -> 'c
    end
    relation Mod 'a,'b,'c -> 'c
        fn mod a,b : ['a, 'b] -> 'c
    end

    relation Eq 'a
        fn /= a,b : ['a, 'a] -> Bool
        fn == a,b : ['a, 'a] -> Bool
    end
    relation Lt 'a
        fn < a,b : ['a, 'a] -> Bool
    end
    relation Gt 'a
        fn > a,b : ['a, 'a] -> Bool
    end
    relation Le 'a
        fn <= a,b : ['a, 'a] -> Bool
    end
    relation Ge 'a
        fn >= a,b : ['a, 'a] -> Bool
    end

    relation Ord 'a where Eq 'a + Lt 'a
    end

    fn max 'a a,b : ['a, 'a] -> 'a where Ord 'a =
        if a < b then b else a
    
    fn min 'a a,b : ['a, 'a] -> 'a where Ord 'a =
        if a < b then a else b
    
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
`,V=x({__name:"RefBasics",setup(M){const f=r(`routine main =
    use arith.int of (+),(<) in begin
        io.println str.cat "1 + 1 = ", fmt.int_to_str 1 + 1;
        io.println str.cat "1 < 2 = ", fmt.bool_to_str 1 < 2;
        io.println str.cat "head of 'abc' is ", fmt.char_to_str str.head "abc";
        use list of (::),([]) in
        io.println str.cat "head of [1,2,3] is ", fmt.int_to_str option.unwrap list.head 1::2::3:: []
    end
`),b=r(`routine r _: [()] -> () =
    io.println "something"
fn f = r ()

routine main = f
`),c=r(`routine print_int i: [Int] -> () = io.println (fmt.to_string i)

fn returns_anything 'a,'b x: ['a] -> 'b =
    panic.panic "don't call this"

routine main = print_int 1
`),p=r(`routine main =
    use ops in
    let r = begin
        let mut x = 1;  /* variable definition statement */
        if x < 2 then begin  /* if statement */
            x <- 2;  /* assignment statement */
        end;
        while x < 10 then begin  /* while statement */
            x <- x + 1;
        end;
        x  /* value of the sequence */
    end in
    io.println "r = " * (fmt.to_string r)
`),m=r(`infix <+> 3 4
infix <-> 3 4
data MyInt = MyInt Int

fn <+> mi1, mi2 =
    let MyInt x = mi1 in
    let MyInt y = mi2 in
    use arith.int in
    MyInt x + y

fn <-> mi1, mi2 =
    let MyInt x = mi1 in
    let MyInt y = mi2 in
    use arith.int in
    MyInt x - y


routine main =
    let MyInt result = (MyInt 1) <-> (MyInt 2) <+> (MyInt 3) in result
`),h=r(`routine main =
    use ops of (+) in
    let add_1 = (+) 1 in
    add_1 2
`),I=r(`data BTree = Leaf Int | Node BTree,BTree

fn left_most t = case t of
    | Leaf x -> x
    | Node l,r -> left_most l
end

data MyInt = MyInt Int

fn unwrap mi = let MyInt i = mi in i

routine main =
    use ops of (+) in
    let tree = Node (Node (Leaf 1), (Leaf 2)),(Leaf 3) in
    let mi = MyInt 3 in
    (unwrap mi) + (left_most tree)
`);return(q,o)=>(w(),B(v,null,[e(y,null,{default:t(()=>[n("Basics")]),_:1}),e(u,null,{default:t(()=>[n("Primitives and Predefined")]),_:1}),e(l,null,{default:t(()=>[n(" Four kinds of primitives are supported, namely Int (a 64-bit machine integer), Float (a 64-bit machine float), Str (a UTF-8 encoded string) and Bool. Also, a number of types and operations on them are pre-defined. Following is a copy of those definitions. ")]),_:1}),e(g,{class:"q-ma-md"},{default:t(()=>[e(_(F),{value:_(d),"onUpdate:value":o[0]||(o[0]=i=>S(d)?d.value=i:null),lang:"txt",readonly:"",style:{height:"500px","font-size":"1em"},"print-margin":!1},null,8,["value"])]),_:1}),e(l,null,{default:t(()=>[n(" They can be used like this ")]),_:1}),e(s,{modelValue:f.value,"onUpdate:modelValue":o[1]||(o[1]=i=>f.value=i),height:300},null,8,["modelValue"]),e(u,null,{default:t(()=>[n("Defining Functions")]),_:1}),e(l,null,{default:t(()=>[n(" There are two kinds of what we usually call functions. One is the simple, good pure function, which are defined using "),e(a,null,{default:t(()=>[n("fn")]),_:1}),n(" keyword; The other one is impure function that does IO and mutate states, which are defined using "),e(a,null,{default:t(()=>[n("routine")]),_:1}),n(" keyword. Of course, an impure function cannot be called from a pure function. ")]),_:1}),e(s,{modelValue:b.value,"onUpdate:modelValue":o[2]||(o[2]=i=>b.value=i),height:300,"initial-split":40},null,8,["modelValue"]),e(l,null,{default:t(()=>[n(' Since pure functions always returns the same thing when called with the same arguments, pure functions with zero arguments are allowed to be defined. They can be called without supplying any arguments. On the other hand, an impure routine must have at least a unit type parameter. The only exception is the main function, since it will not be "called" by anyone. ')]),_:1}),e(l,null,{default:t(()=>[n(" Type annotation can be supplied to a function definition. Genieric type variables are led by a single quote, and is placed after function name. ")]),_:1}),e(s,{modelValue:c.value,"onUpdate:modelValue":o[3]||(o[3]=i=>c.value=i),height:200},null,8,["modelValue"]),e(u,null,{default:t(()=>[n("Expressions and Statements")]),_:1}),e(l,null,{default:t(()=>[n(" The body of all functions are expressions. Calling a routine is also an expression. To write imperative-style programs, one can use "),e(a,null,{default:t(()=>[n("begin")]),_:1}),n(" and "),e(a,null,{default:t(()=>[n("end")]),_:1}),n(" keywords. Between "),e(a,null,{default:t(()=>[n("begin")]),_:1}),n(" and "),e(a,null,{default:t(()=>[n("end")]),_:1}),n(", one can write a sequence of statements, seperated by semicolon. An optional expression can be put at the end of the sequence of statements as the value of the sequence. ")]),_:1}),e(s,{modelValue:p.value,"onUpdate:modelValue":o[4]||(o[4]=i=>p.value=i),height:300},null,8,["modelValue"]),e(l,null,{default:t(()=>[n(" As in the code above, comments are c-styled. A comment is surrounded by "),e(a,null,{default:t(()=>[n("/*")]),_:1}),n(" and "),e(a,null,{default:t(()=>[n("*/")]),_:1}),n(". They can be put anywhere an expression or statement is expected. ")]),_:1}),e(l,null,{default:t(()=>[n(" Expressions with infix operators are parsed according to those operators' precedence. For example, let's say "),e(a,null,{default:t(()=>[n("+, -")]),_:1}),n(" has left precedence 4 and right precedence 3 , then "),e(a,null,{default:t(()=>[n("1+2-3+4")]),_:1}),n(" would be parsed into "),e(a,null,{default:t(()=>[n("((1+2)-3)+4")]),_:1}),n('. Intuitively, the right precedence is higher means that the opertator combines "tighter" with oerpands on its right. ')]),_:1}),e(l,null,{default:t(()=>[n(" Helo's application operator (which is just a space) has zero precedence and is right-associative. This means that "),e(a,null,{default:t(()=>[n("f g h 1 + 2")]),_:1}),n(" would be parsed into "),e(a,null,{default:t(()=>[n("f(g(h(1 + 2)))")]),_:1}),n(". Precedence of common arithmatic operators are predefined in Helo. Precedence of user-defined infix operators can be defined using "),e(a,null,{default:t(()=>[n("infix")]),_:1}),n(" keyword. ")]),_:1}),e(s,{modelValue:m.value,"onUpdate:modelValue":o[5]||(o[5]=i=>m.value=i),height:500},null,8,["modelValue"]),e(u,null,{default:t(()=>[n("Partial Application")]),_:1}),e(l,null,{default:t(()=>[n(" Though currying is not supported, partial application is handy enough. ")]),_:1}),e(s,{modelValue:h.value,"onUpdate:modelValue":o[6]||(o[6]=i=>h.value=i),height:200},null,8,["modelValue"]),e(u,null,{default:t(()=>[n("Algebraic Data Types")]),_:1}),e(l,null,{default:t(()=>[n(" Algebraic data types, or tagged unions, can be defined using the "),e(a,null,{default:t(()=>[n("data")]),_:1}),n(" keyword. Of course, they can be recursived defined. To desconstruct a tagged union, one can use "),e(a,null,{default:t(()=>[n("case of")]),_:1}),n(' expression. If non of the "arms" are matched successfully at runtime, the program will exit with an error message. If the tagged union only contains one variant, '),e(a,null,{default:t(()=>[n("let")]),_:1}),n(" expression can also be useful. ")]),_:1}),e(s,{modelValue:I.value,"onUpdate:modelValue":o[7]||(o[7]=i=>I.value=i),height:400},null,8,["modelValue"])],64))}}),G=T(V,[["__file","RefBasics.vue"]]);export{G as default};
