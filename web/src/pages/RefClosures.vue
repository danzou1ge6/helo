<template>
  <TextHeader1>Closures</TextHeader1>
  <TextBody1>
    A closures captures locals in its surrounding function by refering to them,
    and store those values in their environment.
    <TextCode>let fn</TextCode> and <TextCode>let routine</TextCode> keyword can be used to define
    pure and impure closures, respectively.
  </TextBody1>
  <EmbededPlayGround v-model="closureCode" :height="400"></EmbededPlayGround>

  <TextBody1>
    A named closure can be recursive.
  </TextBody1>
  <EmbededPlayGround v-model="recursiveClosureCode" :height="300"></EmbededPlayGround>

  <TextHeader2>Anonymous Closures</TextHeader2>
  <TextBody1>
    To avoid naming a closure, anonymous closure can be created with <TextCode>\x,y -> ...</TextCode> syntax.
    For the impure case, that would be <TextCode>\'x,y -> ...</TextCode>.
  </TextBody1>
  <EmbededPlayGround v-model="anonymousClosureCode" :height="400"></EmbededPlayGround>
  <TextBody1>
    As shown above, type annotation can be supplied to closure as well.
    A closure shares the same generic type parameters as its parent.
  </TextBody1>

  <TextHeader2>Nested Closures</TextHeader2>
  <TextBody1>
    Closures can be nested.
  </TextBody1>
  <EmbededPlayGround v-model="nestedClosureCode" :height="300"></EmbededPlayGround>
</template>
<script setup lang="ts">
import { ref } from 'vue'
import TextHeader1 from 'src/components/TextHeader1.vue'
import TextHeader2 from 'src/components/TextHeader2.vue'
import TextBody1 from 'src/components/TextBody1.vue'
import TextCode from 'src/components/TextCode.vue'
import EmbededPlayGround from 'src/components/EmbededPlayGround.vue'

const closureCode = ref(
`fn f x =
    use arith.int of (+) in
    let fn g y: [Int] -> Int = x + y in g

routine r x =
    use ops of (*) in
    let routine p y =
      io.println (fmt.int_to_str x)
                 * " "
                 * (fmt.int_to_str y)
    in p

routine main =
    let g = f 1 in
    let z = g 2 in
    let p = r 3 in
    p z
`
)

const recursiveClosureCode = ref(
`routine main =
    use arith.int in
    let fn factorial n =
        if n == 0 then 1 else n * (factorial n - 1)
    in
    factorial 4
`
)

const anonymousClosureCode = ref(
`fn f x =
    use arith.int of (+) in
    \\y: [Int] -> Int -> x + y

routine r x =
    use ops of (*) in
    \\'y -> io.println (fmt.int_to_str x)
                       * " "
                       * (fmt.int_to_str y)

routine main =
    let g = f 1 in
    let z = g 2 in
    let p = r 3 in
    p z
`
)

const nestedClosureCode = ref(
`fn f x =
    \\y: [Int] -> [Int] -> Int ->
    /* closure above must also captures x because
     * its "child closure" captures x
     */
        \\z -> x  /* x here refer to f's parameter */

routine main =
    let h = (f 1) 2 in
    h 3
`
)
</script>
