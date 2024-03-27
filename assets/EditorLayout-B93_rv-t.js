import{Q as w,a as P,R as E}from"./QBtn-DKy_yooC.js";import{Q as A,a as I,b as B,N as H,c as C,d as L,e as M,f as R}from"./NavigationButtons-1B0Mayvj.js";import{Q as F,a as O,b as _}from"./QScrollObserver-C9H9M275.js";import{Q,a as V,P as D}from"./PlayGround-DJiHJlkP.js";import{i as U,q as z,d as q,o as v,r as s,a as l,c as m,w as e,b as n,e as p,f as K,g as j,F as $,h as T,j as G,k as J,t as W,_ as X}from"./index-DHLu7VNo.js";function Y(){return U(z)}const Z=`/* An AVL tree implementation
 * 
 * Usage:
 *   There are two commands
 *   - \`i<x>\` where <x> is an integer: Insert <x> into the tree
 *   - \`d<x>\` where <x> is an integer: Delete <x> from the tree, if one exists
 *   After each command is executed, the structure of the tree will be printed as
 *     \`\`\`
 *     <root_value>, h = <tree_height>
 *      <sub_tree>
 *      <sub_tree>
 *     \`\`\`
 */

use ops
use result

infix :: 10 9
infix // 9 10

data Avl[T] = Empty
             | Node T, Avl[T], Avl[T], Int

fn height t = case t of
    | Empty -> 0
    | Node _,_,_,h -> h
    end

fn balance_factor t = case t of
    | Node _, left, right, _ -> (height left) - (height right)
    | Empty -> 0
    end

fn node v,left,right = Node v,left,right,1 + (max (height left),(height right))

fn rotate_left t = case t of
    | Node x,a,(Node y,b,c,_),_ -> node y,(node x,a,b),c
    end

fn rotate_right t = case t of
    | Node x,(Node y,a,b,_),c,_ -> node y,a,(node x,b,c)
    end

fn balance t = case t of
    | Empty -> t
    | Node x,left,right,_ ->
        let factor = balance_factor t in
        if factor > 1 then
            if (balance_factor left) >= 0 then
                rotate_right t
            else
                rotate_right (node x, (rotate_left left), right)
        else if factor < -1 then
            if (balance_factor right) <= 0 then
                rotate_left t
            else
                rotate_left (node x, left, (rotate_right right))
        else
            t
    end

fn insert x,t: [T, Avl[T]] -> Avl[T] where Ord T =
    case t of
    | Empty -> Node x,Empty,Empty,1
    | Node v,left,right,h ->
        if x < v then
            let new_left = insert x,left in
            balance (node v,new_left,right)
        else if v < x then
            let new_right = insert x,right in
            balance (node v,left,new_right)
        else
            Node x,left,right,h
    end

fn find_min t: [Avl[T]] -> T = case t of
    | Node v,Empty,_,_ -> v
    | Node _,left,_,_ -> find_min left
    end

fn delete x,t: [T, Avl[T]] -> Avl[T] where Ord T =
    case t of
    | Empty -> t
    | Node v,left,right,h ->
        if x < v then
            let new_left = delete x,left in
            balance (node v,new_left,right)
        else if v < x then
            let new_right = delete x,right in
            balance (node v,left,new_right)
        else
            case left of
            | Empty -> right
            | _ ->
                case right of
                | Empty -> left
                | _ ->
                    let min_val = find_min right in
                    let new_right = delete min_val,right in
                    balance (node min_val,left,new_right)
                end
            end
    end

fn contains x,t: [T, Avl[T]] -> Bool where Ord T =
    case t of
    | Empty -> false
    | Node v,left,right,_ ->
        if x < v then
            contains x,left
        else if v < x then
            contains x,right
        else
            x == v
    end

fn str_repeat c,n: [Char, Int] -> Str =
    let fn helper s,c,n: [Str, Char, Int] -> Str =
        if n == 0 then
            s
        else
            helper ((fmt.to_string c) * s),c,(n - 1)
    in
        helper "",c,n

fn debug t,indent: [Avl[T], Int] -> Str where fmt.ToString T =
    case t of
    | Empty -> (str_repeat ' ',indent) * "()\\n"
    | Node v,Empty,Empty,h ->
        (str_repeat ' ',indent) * (fmt.to_string v) * ", h = " * (fmt.to_string h) * "\\n"
    | Node v,left,right,h ->
        (str_repeat ' ',indent) * (fmt.to_string v) * ", h = " * (fmt.to_string h) * "\\n"
        * (debug left, (indent + 1))
        * (debug right, (indent + 1))
    end

fn :: x,t: [T, Avl[T]] -> Avl[T] where Ord T = insert x,t
fn // t,x: [Avl[T], T] -> Avl[T] where Ord T = delete x,t

data Command = Insert Int | Delete Int
data ParseError = ParseError Str,Str

fn parse_digit s: [Str] -> (Str, Result[Int, ParseError]) =
    if str.some s then
        case str.head s of
        | '0' -> ((str.tail s), Ok 0)
        | '1' -> ((str.tail s), Ok 1)
        | '2' -> ((str.tail s), Ok 2)
        | '3' -> ((str.tail s), Ok 3)
        | '4' -> ((str.tail s), Ok 4)
        | '5' -> ((str.tail s), Ok 5)
        | '6' -> ((str.tail s), Ok 6)
        | '7' -> ((str.tail s), Ok 7)
        | '8' -> ((str.tail s), Ok 8)
        | '9' -> ((str.tail s), Ok 9)
        | _   -> (s, Err ParseError s,"digit")
        end
    else (s, Err ParseError s,"digit")

fn parse_int s,x: [Str, Int] -> (Str, Int) =
    case parse_digit s of
    | (s1, Ok digit) ->
        parse_int s1,(x * 10 + digit)
    | (s1, Err e   ) ->
        (s1, x)
    end

fn parse_command s: [Str] -> (Str, Result[Command, ParseError]) =
    if str.some s then
        case str.head s of
        | 'i' -> let (s1, x) = parse_int (str.tail s),0 in (s1, Ok Insert x)
        | 'd' -> let (s1, x) = parse_int (str.tail s),0 in (s1, Ok Delete x)
        | _ -> (s, Err ParseError s, "command")
        end
    else (s, Err ParseError s,"command")

routine main =
    let mut tree1 = 1::4::2::6::5::Empty // 1 // 6 in begin
    while true then begin
        let line = io.readline ();
        case parse_command line of
        | (_, Ok Insert x) -> begin
            tree1 <- insert x,tree1;
            io.println (debug tree1,0)
            end
        | (_, Ok Delete x) -> begin
            tree1 <- delete x,tree1;
            io.println (debug tree1,0)
            end
        | (_, Err e) -> io.println "Bad command"
        end
    end
    end

`,nn=`/* An infix expression evaluator implementation
 *
 * Addition, subtraction, multiplication and division of float numbers are supported.
 * Note that an naive method to parse float here is applied here, which is definitely not the most accurate.
 */

data ParseError = ParseError Str,Str

use option
use result
use ops
use io
use convert
use fmt of to_string

fn parse_digit s: [Str] -> (Str, Result[Int, ParseError]) =
    if str.some s then
        case str.head s of
        | '0' -> ((str.tail s), Ok 0)
        | '1' -> ((str.tail s), Ok 1)
        | '2' -> ((str.tail s), Ok 2)
        | '3' -> ((str.tail s), Ok 3)
        | '4' -> ((str.tail s), Ok 4)
        | '5' -> ((str.tail s), Ok 5)
        | '6' -> ((str.tail s), Ok 6)
        | '7' -> ((str.tail s), Ok 7)
        | '8' -> ((str.tail s), Ok 8)
        | '9' -> ((str.tail s), Ok 9)
        | _   -> (s, Err ParseError s,"digit")
        end
    else (s, Err ParseError s,"digit")

data Op = OpAdd | OpSub | OpMul | OpDiv

fn op_precedence op: [Op] -> (Int, Int) =
    case op of
    | OpAdd -> (3, 4)
    | OpSub -> (3, 4)
    | OpMul -> (7, 8)
    | OpDiv -> (7, 8)
    end

fn op_precedence_left op = let (left, _) = op_precedence op in left
fn op_precedence_right op = let (_, right) = op_precedence op in right

fn parse_operator s: [Str] -> (Str, Result[Op, ParseError]) =
    if str.some s then
        case str.head s of
        | '+' -> ((str.tail s), Ok OpAdd)
        | '-' -> ((str.tail s), Ok OpSub)
        | '*' -> ((str.tail s), Ok OpMul)
        | '/' -> ((str.tail s), Ok OpDiv)
        | _   -> (s, Err ParseError s,"operator")
        end
    else (s, Err ParseError s,"operator")

fn parse_float s: [Str] -> (Str, Result[Float, ParseError]) =
    let fn parse_int s,x: [Str, Float] -> (Str, Float) =
        case parse_digit s of
        | (s1, Ok digit) ->
          parse_int s1,(x * 10.0 + (from digit): Float)
        | (s1, Err e   ) ->
          (s1, x)
        end
    in
    let fn parse_mantissa s,x,n: [Str, Float, Float] -> (Str, Float) =
        case parse_digit s of
        | (s1, Ok digit) ->
          parse_mantissa s1,(x + n * (from digit): Float),(n * 0.1)
        | (s1, Err e   ) ->
         (s1, x)
        end
    in
    
    case parse_digit s of
        | (_s, Ok _x) ->
          let (s1, x) = parse_int s,0.0 in
          if str.some s1 then
              case str.head s1 of
              | '.'       ->
                let (s2, x) = parse_mantissa (str.tail s1),x,0.1 in
                (s2, Ok x)
              | otherwise ->
                (s1, Ok x)
              end
          else (s1, Ok x)
        | otherwise -> (s, Err ParseError s,"float")
    end

data Expr = Apply Op,Expr,Expr
          | Lit Float

fn build_infix_expr lhs,op,s: [Expr, Op, Str] -> (Str, Result[Expr, ParseError]) =
    let p_right = op_precedence_right op in
    case parse_expr_precedenced s,p_right of
    | (s1, Ok rhs)  -> (s1, Ok Apply op,lhs,rhs)
    | (s1, Err e) -> (s1, Err e)
    end

fn parse_tag tag,s: [Char, Str] -> (Str, Result[Char, ParseError]) =
    if str.some s then
        if (str.head s) == tag then
            ((str.tail s), Ok tag)
        else
            (s, Err ParseError s,(to_string tag))
    else
        (s, Err ParseError s,(to_string tag))

fn parse_expr_lit s: [Str] -> (Str, Result[Expr, ParseError]) =
    case parse_float s of
    | (s1, Ok float) -> (s1, Ok (Lit float))
    | (s1, Err e)    -> (s1, Err e)
    end

fn parse_prefix_expr s: [Str] -> (Str, Result[Expr, ParseError]) =
    case parse_tag '(',s of
    | (s1, Ok _) ->
      case parse_expr s1 of
      | (s2, Ok expr) ->
        case parse_tag ')',s2 of
        | (s3, Ok _) -> (s3, Ok expr)
        | (s3, Err e) -> (s3, Err e)
        end
      | (s2, Err e) -> (s2, Err e)
      end
    | _  ->
      case parse_tag '-',s of
      | (s1, Ok _) ->
        let p_right = op_precedence_right OpSub in
        case parse_expr_precedenced s1,p_right of
        | (s2, Ok expr) -> (s2, Ok Apply OpSub,(Lit 0.0),expr)
        | (s2, Err e) -> (s2, Err e)
        end
      | _ -> parse_expr_lit s
      end
    end

fn parse_expr_precedenced s,prec: [Str, Int] -> (Str, Result[Expr, ParseError]) =
    let fn parse s,lhs,prec: [Str, Expr, Int] -> (Str, Result[Expr, ParseError]) =
        case parse_operator s of
        | (s1, Ok op) ->
          if (op_precedence_left op) >= prec then
              case build_infix_expr lhs,op,s1 of
              | (s2, Ok expr) -> parse s2,expr,prec
              | (s2, Err e)   -> (s2, Err e)
              end
          else
              (s, Ok lhs)
        | (s1, Err e)   -> (s, Ok lhs)
        end
    in
    case parse_prefix_expr s of
        | (s1, Ok lhs)  -> parse s1,lhs,prec
        | (s1, Err e) -> (s1, Err e)
    end

fn parse_expr s = parse_expr_precedenced s,0

fn parse_expr_finish s: [Str] -> Result[Expr, ParseError] =
    case parse_expr s of
    | (s1, Ok expr) ->
       if (str.len s1) /= 0 then
           Err ParseError s1,"expression-finished"
       else
           Ok expr
    | (s1, Err e) -> Err e
    end
            

fn eval_expr expr: [Expr] -> Float =
    case expr of
    | Lit x -> x
    | Apply op,lhs,rhs ->
      let lhs = eval_expr lhs in
      let rhs = eval_expr rhs in
      case op of
      | OpAdd -> lhs + rhs
      | OpSub -> lhs - rhs
      | OpMul -> lhs * rhs
      | OpDiv -> lhs / rhs
       end
    end

fn str_repeat c,n: [Char, Int] -> Str =
    let fn helper s,c,n: [Str, Char, Int] -> Str =
        if n == 0 then
            s
        else
            helper ((to_string c) * s),c,(n - 1)
    in
        helper "",c,n

routine print_parse_error e,s: [ParseError, Str] -> () =
    let ParseError at,name = e in
    begin
        println (str_repeat ' ',((str.len s) - (str.len at)))
                 * "| <- Error parsing "
                 * name
                 * " Here"
    end

routine print_result x: [Float] -> () =
    println "= " * (to_string x)

routine main = begin
    while true then begin
        let line = readline ();
        case parse_expr_finish line of
        | Ok expr  -> print_result (eval_expr expr)
        | Err e  -> print_parse_error e,line
        end
    end
end
`,en=`relation Bigger A,B end

module eat_rel
    relation Eat A,B,C -> C where super.Bigger A,B
        routine eat a,b : [A,B] -> C
    end
end

relation Carnivorous A,C -> C
    routine car_eat B a,b: [A,B] -> C where Bigger A,B
end

relation Poop B end

relation PoopEating A,C -> C
    routine eat_poop B a,b: [A,B] -> C where Poop B
end

/* Implement \`Eat(a, b, c)\` for all \`(a, b, c)\` such that \`PoopEating(a, c)\` and \`Poop(b)\` and \`Bigger(a, b)\`
 */
instance eat_rel.Eat A,B,C where PoopEating A,C + Poop B + Bigger A,B
    routine eat a,b = eat_poop a,b
end

/* Implement \`Eat(a, b, c)\` for all \`(a, b, c)\` such that \`Carnivorous(a, c)\` and \`Bigger(a, b)\`
 */
instance eat_rel.Eat A,B,C where Carnivorous A,C + Bigger A,B
    routine eat a,b = car_eat a,b
end

data Neko = Neko Str
data NekoPoop = NekoPoop Int
data Mickey = Mickey Str
data MickeyPoop = MickeyPoop Int
data Rice = Rice Str
data Bacteria = Bacteria Str
data CO2 = CO2 Int

instance Poop NekoPoop end
instance Poop MickeyPoop end
instance Bigger Bacteria,P where Poop P end

instance PoopEating Bacteria,CO2
    routine eat_poop a,b = begin
        io.println "Bacteria eat poop";
        CO2 1
    end
end

instance Bigger Neko,Mickey end
instance Bigger Mickey,Rice end

instance Carnivorous Neko,NekoPoop
    routine car_eat a,b = begin
        io.println "Neko eat";
        NekoPoop 1
    end
end

instance eat_rel.Eat Mickey,Rice,MickeyPoop
    routine eat a,b = begin
        io.println "Mickey eats rice";
        MickeyPoop 1
    end
end


/* test comment 1 */

routine main =
    use eat_rel of eat in
    let neko = Neko "1" in
    /* test comment 2 */
    let rice = Rice "3" /* test comment 3 */ in
    let bacteria = Bacteria "4" in
    let mickey = Mickey "2" in begin
        /* comment test 4 */
        let neko_poop = eat neko,mickey;
        let mickey_poop = eat mickey,rice;
        eat bacteria,neko_poop
    end

`,tn=`use list of (::) , ([])
use ops

infix :: 10 9

fn head xs = case xs of
    | x::xs -> option.Some x
    | [] -> option.None
end

routine main =
    use ops of (+) in
    let xs = 1::(1 + 1)::3:: [] in
    let x = head xs in begin
        case x of
        | option.Some x ->
          io.println "Head is " * (fmt.int_to_str x)
        | option.None -> panic.panic "impossible"
        end ;
        case xs of
        | 1::2::x:: [] -> io.println "Third is " * (fmt.to_string x)
        | _ -> panic.panic "impossible"
        end
    end

`,rn=`infix >>= 9 10

use result
use ops

fn >>= x,f = case x of
    | Ok x -> f x
    | Err e -> Err e
end

fn return x = Ok x

fn plus1_conditioned x: [Int] -> Result[Int,Str] =
    if x < 10 then
        Err "too small"
    else if x > 15 then
        Err "too big"
    else
        Ok (x + 1)

fn f x = 
    (plus1_conditioned x) >>= \\y ->
    (plus1_conditioned x) >>= \\x ->
    (plus1_conditioned y) >>= \\x ->
    return x

routine main =
    let x = 9 in
    let y = 13 in begin
        case f x of
        | Ok r -> io.println "Ok " * (fmt.to_string r)
        | Err e -> io.println "Err " * e
        end ;
        case f y of
        | Ok r -> io.println "Ok " * (fmt.to_string r)
        | Err e -> io.println "Err " * e
        end ;
    end


`,sn=`/* An implementation of Skew Binomial Heap as described in _Purely Functional Datastructures_ by
 * Okasaki
 *
 * Usage:
 *   There are two commands:
 *   - \`i<x>\` where <x> is an integer: Insert <x> into the heap
 *   - \`p\`: Pop the smallest integer from the heap, if heap is non-empty
 *   After each command is executed, the content of the heap will be printed in increasing sequence
 */


use list of ([]), (::), List, rev
use ops
use option of Option, Some, None
use result

infix :- 10 9

data Node[T] = Node Int,T,List[T],List[Node[T]]

data Heap[T] = Heap List[Node[T]]

fn is_empty h =
    let Heap ts = h in
    list.is_empty ts

fn rank node = let Node r,_,_,_ = node in r

fn root node = let Node _,x,_,_ = node in x

fn link t1,t2: [Node[T], Node[T]] -> Node[T] where Ord T =
    let Node r,x1,xs1,c1 = t1 in
    let Node _,x2,xs2,c2 = t2 in
    if x2 < x1 then
        Node r + 1, x2, xs2, t1 :: c2
    else
        Node r + 1, x1, xs1, t2 :: c1

fn skew_link x,t1,t2: [T, Node[T], Node[T]] -> Node[T] where Ord T =
    let Node r,y,ys,c = link t1,t2 in
    if y < x then
        Node r, y, x :: ys, c
    else
        Node r, x, y :: ys, c

fn ins_tree t,ts : [Node[T], List[Node[T]]] -> List[Node[T]] where Ord T =
    case ts of
    | [] -> t :: []
    | t2 :: ts ->
        if (rank t) < (rank t2) then
            t :: t2 :: ts
        else
            ins_tree (link t, t2), ts
    end

fn merge_trees h1,h2: [List[Node[T]], List[Node[T]]] -> List[Node[T]] where Ord T =
    case (h1, h2) of
    | (ts1, []) -> h1
    | ([], ts2) -> h2
    | (t1 :: ts1, t2 :: ts2) ->
        if (rank t1) < (rank t2) then
            t1 :: (merge_trees ts1, t2 :: ts2)
        else if (rank t2) < (rank t1) then
            t2 :: (merge_trees t1 :: ts1, ts2)
        else
            ins_tree (link t1,t2), (merge_trees ts1,ts2)
    end

fn normalize h: [List[Node[T]]] -> List[Node[T]] where Ord T =
    case h of
    | [] -> h
    | t :: ts -> ins_tree t, ts
    end

fn insert x,h: [T, Heap[T]] -> Heap[T] where Ord T =
    case h of
    | Heap t1 :: t2 :: ts -> 
        if (rank t1) == (rank t2) then
            Heap (skew_link x,t1,t2) :: ts
        else
            Heap (Node 0,x,[],[]) :: t1 :: t2 :: ts
    | Heap ts ->
        Heap (Node 0,x,[],[]) :: ts
    end

fn merge h1,h2: [Heap[T], Heap[T]] -> Heap[T] where Ord T =
    let Heap h1 = h1 in
    let Heap h2 = h2 in
    Heap merge_trees (normalize h1), (normalize h2)

fn find_min h: [Heap[T]] -> Option[T] where Ord T =
    case h of
    | Heap [] -> None
    | Heap t :: [] -> Some root t
    | Heap t :: ts ->
        let x = root t in
        case find_min Heap ts of
        | Some y -> Some min x,y
        | None -> None
        end
    end

fn delete_min h: [Heap[T]] -> Option[Heap[T]] where Ord T =
    case h of
    | Heap [] -> None
    | Heap ts ->
        let fn get_min ts: [List[Node[T]]] -> (Node[T], List[Node[T]]) =
            case ts of
            | t :: [] -> (t, [])
            | t :: ts ->
                let (t1, ts1) = get_min ts in
                if (root t1) < (root t) then
                    (t1, t :: ts1)
                else
                    (t, ts)
            end
        in
        let fn insert_all ts, h: [List[T], Heap[T]] -> Heap[T] =
            case ts of
            | [] -> h
            | t :: ts -> insert_all ts,(insert t,h)
            end
        in
        let ((Node _,x,xs,c), ts1) = get_min ts in
        Some insert_all xs, (Heap merge_trees (rev c),(normalize ts1))
    end

fn empty : [] -> Heap[T] = Heap []
fn :- x,h : [T, Heap[T]] -> Heap[T] where Ord T = insert x,h

data Command = Insert Int | Pop
data ParseError = ParseError Str,Str

fn parse_digit s: [Str] -> (Str, Result[Int, ParseError]) =
    if str.some s then
        case str.head s of
        | '0' -> ((str.tail s), Ok 0)
        | '1' -> ((str.tail s), Ok 1)
        | '2' -> ((str.tail s), Ok 2)
        | '3' -> ((str.tail s), Ok 3)
        | '4' -> ((str.tail s), Ok 4)
        | '5' -> ((str.tail s), Ok 5)
        | '6' -> ((str.tail s), Ok 6)
        | '7' -> ((str.tail s), Ok 7)
        | '8' -> ((str.tail s), Ok 8)
        | '9' -> ((str.tail s), Ok 9)
        | _   -> (s, Err ParseError s,"digit")
        end
    else (s, Err ParseError s,"digit")

fn parse_int s,x: [Str, Int] -> (Str, Int) =
    case parse_digit s of
    | (s1, Ok digit) ->
        parse_int s1,(x * 10 + digit)
    | (s1, Err e   ) ->
        (s1, x)
    end

fn parse_command s: [Str] -> (Str, Result[Command, ParseError]) =
    if str.some s then
        case str.head s of
        | 'i' -> let (s1, x) = parse_int (str.tail s),0 in (s1, Ok Insert x)
        | 'p' -> ((str.tail s), Ok Pop)
        | _ -> (s, Err ParseError s, "command")
        end
    else (s, Err ParseError s,"command")

fn format_heap h: [Heap[T]] -> Str where Ord T + fmt.ToString T =
    case find_min h of
    | None -> ""
    | Some x ->
        let h1 = option.unwrap delete_min h in
        let h1_formatted = format_heap h1 in
        (fmt.to_string x) * " " * h1_formatted
    end


routine main =
    let mut h = option.unwrap delete_min 4 :- 3 :- 8 :- 1 :- 2 :- empty in begin
    while true then begin
        let line = io.readline ();
        case parse_command line of
        | (_, Ok Insert x) -> begin
            h <- insert x,h;
            io.println format_heap h
            end
        | (_, Ok Delete x) -> begin
            h <- case delete_min h of
                 | None -> h
                 | Some h1 -> h1
                 end;
            io.println format_heap h
            end
        | (_, Err e) -> io.println "Bad command"
        end
    end
    end

`,an={class:"q-pa-sm"},on=q({name:"EditorLayout",__name:"EditorLayout",props:{exampleName:{}},setup(b){const u=b;v(()=>{if(u.exampleName!==void 0){const c=a.value.findIndex(t=>t[0]===u.exampleName);c!==-1&&(i.value=c,k.value=!0)}});const N=Y(),x=s(N.screen.height-50);v(()=>{window.addEventListener("resize",()=>{x.value=window.innerHeight-50})});const g=s(50),k=s(!1),o=s(!1),a=s([["New",""],["AVLTree",Z],["Calculator",nn],["RelationExample",en],["InfixConstructorExample",tn],["ErrorMonadExample",rn],["SkewBinomialHeap",sn]]),i=s(0);function S(){o.value=!o.value}const d=s(""),f=s(!1);function y(){a.value.push([d.value,""]),f.value=!1,i.value=a.value.length-1}return(c,t)=>(l(),m(A,{view:"hHh Lpr lFf"},{default:e(()=>[n(C,{style:{height:"50px"}},{default:e(()=>[n(I,null,{default:e(()=>[n(w,{flat:"",dense:"",icon:"menu","aria-label":"Menu",onClick:S}),n(B,{style:{}},{default:e(()=>[p(" Helo ")]),_:1}),n(H)]),_:1})]),_:1}),n(M,{modelValue:o.value,"onUpdate:modelValue":t[2]||(t[2]=r=>o.value=r),bordered:""},{default:e(()=>[n(F,{header:""},{default:e(()=>[p("Example Scripts")]),_:1}),n(L,null,{default:e(()=>[(l(!0),K($,null,j(a.value,(r,h)=>T((l(),m(O,{clickable:"",key:h,active:h==i.value,"active-class":"bg-blue-1"},{default:e(()=>[n(_,{onClick:ln=>i.value=h},{default:e(()=>[p(W(r[0]),1)]),_:2},1032,["onClick"])]),_:2},1032,["active"])),[[E]])),128)),T((l(),m(O,{clickable:""},{default:e(()=>[n(_,{avatar:""},{default:e(()=>[n(P,{class:"text-primary",name:"add"})]),_:1}),n(_,{class:"text-primary"},{default:e(()=>[p("Add New File")]),_:1}),n(Q,{modelValue:f.value,"onUpdate:modelValue":t[1]||(t[1]=r=>f.value=r),style:{"font-size":"1.2rem"},anchor:"top right"},{default:e(()=>[G("div",an,[n(V,{autofocus:"",placeholder:"File Name",dense:"",modelValue:d.value,"onUpdate:modelValue":t[0]||(t[0]=r=>d.value=r),onKeyup:J(y,["enter"])},null,8,["modelValue"])])]),_:1},8,["modelValue"])]),_:1})),[[E]])]),_:1})]),_:1},8,["modelValue"]),n(R,null,{default:e(()=>[n(D,{"page-height":x.value,modelValue:a.value[i.value][1],"onUpdate:modelValue":t[3]||(t[3]=r=>a.value[i.value][1]=r),"show-output":!0,split:g.value,"onUpdate:split":t[4]||(t[4]=r=>g.value=r),"run-on-mount":k.value,name:a.value[i.value][0]},null,8,["page-height","modelValue","split","run-on-mount","name"])]),_:1})]),_:1}))}}),_n=X(on,[["__file","EditorLayout.vue"]]);export{_n as default};