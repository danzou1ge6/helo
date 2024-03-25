<template>
  <TextHeader1>Relations</TextHeader1>
  <TextBody1>
    To allow ad-hoc polymorphism, we borrow the mathmatical concept of "relation".
    If some methods are implemented for a series of types, then those types satisfy some certain relation.
    Those relations are defined by the <TextCode>relation</TextCode> keyword,
    and those methods are implemented for some concrete types through the <TextCode>instance</TextCode> keyword.
  </TextBody1>
  <EmbededPlayGround v-model="relationCode" :height="500"></EmbededPlayGround>

  <TextHeader2>Dependent</TextHeader2>
  <TextBody1>
    Sometimes, for example, for a relation R(x, y, z), z can be uniquely determined from the value of x and y,
    then z is called dependent on other variables.
    In this case, if the inferer knows the concrete type of x and y,
    it can know the type of z even z has not been infered.
    To express this, we use a <TextCode>-></TextCode> syntax as shown above.
  </TextBody1>

  <TextHeader2>Constrains</TextHeader2>
  <TextBody1>
    Generic type variables in a relation, instance or function can be constrained.
    That is, we can say that some type variables must satisfy certain relations,
    then we can use relevant methods in the function body.
  </TextBody1>
  <EmbededPlayGround v-model="constrainCode" :height="500"></EmbededPlayGround>

  <TextBody1>
    Not only can constrains be added to function signature,
    but they can also be a part of instance definition or relation definition.
    A constrain R(x, y) on a relation means that to implement this relation for some types x = a, y = b,
    R(a, b) must also be implemented.
    A constrain R(a, b) on a instance means that this instance is implemented for all types a, b that satisfies R.
  </TextBody1>
  <EmbededPlayGround v-model="constrainCode2" :height="300" :initial-split="30"></EmbededPlayGround>
  <TextBody1>
    Refer to <q-btn flat color="primary" dense no-caps to="/playground/RelationExample">Relation Example</q-btn>
    in playground for a more complicated example.
  </TextBody1>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import TextHeader1 from 'src/components/TextHeader1.vue'
import TextHeader2 from 'src/components/TextHeader2.vue'
import TextBody1 from 'src/components/TextBody1.vue'
import TextCode from 'src/components/TextCode.vue'
import EmbededPlayGround from 'src/components/EmbededPlayGround.vue'

const iteratorDefCode =
`/* 'b is dependent on 'a */
relation Iterator 'a,'b -> 'b
    fn next it: ['a] -> ('a, 'b)
end

data IntIterator = IntIterator Int

instance Iterator IntIterator,Int
    fn next it =
        let IntIterator x = it in
        use ops of (+) in
        ((IntIterator x + 1), x)
end
`

const relationCode = ref(
  iteratorDefCode + `
routine main =
    let it = IntIterator 1 in
    let (it, x1) = next it in
    let (it, x2) = next it in
    use ops of (*) in
    io.println (fmt.to_string x1) * " " * (fmt.to_string x2)
`)

const constrainCode = ref(
  iteratorDefCode + `
fn take 'a,'b it,n: ['a, Int] -> list.List['b] where Iterator 'a,'b =
    use list in
    use ops of (-),(==) in
    if n == 0 then
        []
    else
        let (it, x) = next it in
        x :: (take it,n - 1)

fn debug_int_list xs: [list.List[Int]] -> Str =
    use ops of (*) in
    use list in
    case xs of
    | x :: xs -> (fmt.to_string x) * " " * (debug_int_list xs)
    | [] -> ""
    end

routine main =
    io.println debug_int_list take (IntIterator 1),3
`
)

const constrainCode2 = ref(
`relation A 'a end
relation B 'a where A 'a end

instance B Int end

routine main = 0
`
)
</script>
