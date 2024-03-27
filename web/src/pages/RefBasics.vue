<template>
  <TextHeader1>Basics</TextHeader1>
  <TextHeader2>Primitives and Predefined</TextHeader2>

  <TextBody1>
    Four kinds of primitives are supported, namely Int (a 64-bit machine integer), Float (a 64-bit machine float),
    Str (a UTF-8 encoded string) and Bool.

    Also, a number of types and operations on them are pre-defined. Following is a copy of those definitions.
  </TextBody1>
  <q-card class="q-ma-md">
    <v-ace-editor v-model:value="builtinDefs" lang="txt" readonly style="height: 500px; font-size: 1em;"
    :print-margin="false"></v-ace-editor>
  </q-card>
  <TextBody1>
    They can be used like this
  </TextBody1>
  <EmbededPlayGround v-model="primitiveCode" :height="300" ></EmbededPlayGround>

  <TextHeader2>Defining Functions</TextHeader2>
  <TextBody1>
    There are two kinds of what we usually call functions.
    One is the simple, good pure function, which are defined using <TextCode>fn</TextCode> keyword;
    The other one is impure function that does IO and mutate states, which are defined using <TextCode>routine</TextCode>
    keyword. Of course, an impure function cannot be called from a pure function.
  </TextBody1>
  <EmbededPlayGround v-model="impureInpureCode" :height="300" :initial-split="40"></EmbededPlayGround>
  <TextBody1>
    Since pure functions always returns the same thing when called with the same arguments,
    pure functions with zero arguments are allowed to be defined.
    They can be called without supplying any arguments.
    On the other hand, an impure routine must have at least a unit type parameter.
    The only exception is the main function, since it will not be "called" by anyone.
  </TextBody1>
  <TextBody1>
    Type annotation can be supplied to a function definition.
    Generic parameters do not need to be declared before they are referenced.
    Instead, they are automatically collected from type annotation:
    All identifier that is not a known type is considered to be a generic type variable.
  </TextBody1>
  <EmbededPlayGround v-model="typeAnnotationCode" :height="200"></EmbededPlayGround>

  <TextHeader2>Expressions and Statements</TextHeader2>
  <TextBody1>
    The body of all functions are expressions.
    Calling a routine is also an expression.
    To write imperative-style programs, one can use <TextCode>begin</TextCode> and <TextCode>end</TextCode> keywords.
    Between <TextCode>begin</TextCode> and <TextCode>end</TextCode>, one can write a sequence of statements,
    seperated by semicolon.
    An optional expression can be put at the end of the sequence of statements as the value of the sequence.
  </TextBody1>
  <EmbededPlayGround v-model="sequenceCode" :height="300"></EmbededPlayGround>
  <TextBody1>
    As in the code above, comments are c-styled. A comment is surrounded by <TextCode>/*</TextCode> and <TextCode>*/</TextCode>.
    They can be put anywhere an expression or statement is expected.
  </TextBody1>
  <TextBody1>
    Expressions with infix operators are parsed according to those operators' precedence.
    For example, let's say <TextCode>+, -</TextCode> has left precedence 4 and right precedence 3 , then
    <TextCode>1+2-3+4</TextCode> would be parsed into <TextCode>((1+2)-3)+4</TextCode>.
    Intuitively, the right precedence is higher means that the opertator combines "tighter" with oerpands
    on its right.
  </TextBody1>
  <TextBody1>
    Helo's application operator (which is just a space) has zero precedence and is right-associative.
    This means that <TextCode>f g h 1 + 2</TextCode> would be parsed into <TextCode>f(g(h(1 + 2)))</TextCode>.
    Precedence of common arithmatic operators are predefined in Helo.
    Precedence of user-defined infix operators can be defined using <TextCode>infix</TextCode> keyword.
  </TextBody1>
  <EmbededPlayGround v-model="precedenceCode" :height="500"></EmbededPlayGround>

  <TextHeader2>Partial Application</TextHeader2>
  <TextBody1>
    Though currying is not supported, partial application is handy enough.
  </TextBody1>
  <EmbededPlayGround v-model="partialApplicationCode" :height="200"></EmbededPlayGround>

  <TextHeader2>Algebraic Data Types</TextHeader2>
  <TextBody1>
    Algebraic data types, or tagged unions, can be defined using the <TextCode>data</TextCode> keyword.
    Of course, they can be recursived defined.
    To desconstruct a tagged union, one can use <TextCode>case of</TextCode> expression.
    If non of the "arms" are matched successfully at runtime, the program will exit with an error message.
    If the tagged union only contains one variant, <TextCode>let</TextCode> expression can also be useful.
  </TextBody1>
  <EmbededPlayGround v-model="adtCode" :height="400"></EmbededPlayGround>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import { VAceEditor } from 'vue3-ace-editor'
import EmbededPlayGround from 'src/components/EmbededPlayGround.vue'
import builtinDefs from '../../../crates/helo_parse/src/builtin_defs.helo?raw'
import TextHeader1 from 'src/components/TextHeader1.vue'
import TextHeader2 from 'src/components/TextHeader2.vue'
import TextBody1 from 'src/components/TextBody1.vue'
import TextCode from 'src/components/TextCode.vue'

const primitiveCode = ref(
`routine main =
    use arith.int of (+),(<) in begin
        io.println str.cat "1 + 1 = ", fmt.int_to_str 1 + 1;
        io.println str.cat "1 < 2 = ", fmt.bool_to_str 1 < 2;
        io.println str.cat "head of 'abc' is ", fmt.char_to_str str.head "abc";
        use list of (::),([]) in
        io.println str.cat "head of [1,2,3] is ", fmt.int_to_str option.unwrap list.head 1::2::3:: []
    end
`
)

const impureInpureCode = ref(
`routine r _: [()] -> () =
    io.println "something"
fn f = r ()

routine main = f
`
)

const typeAnnotationCode = ref(
`routine print_int i: [Int] -> () = io.println (fmt.to_string i)

fn returns_anything x: [A] -> B =
    panic.panic "don't call this"

routine main = print_int 1
`
)

const sequenceCode = ref(
`routine main =
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
`
)

const precedenceCode = ref(
`infix <+> 3 4
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
`
)

const partialApplicationCode = ref(
`routine main =
    use ops of (+) in
    let add_1 = (+) 1 in
    add_1 2
`
)

const adtCode = ref(
`data BTree = Leaf Int | Node BTree,BTree

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
`
)
</script>
