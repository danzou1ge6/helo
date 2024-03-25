<template>
  <TextHeader1>Introduction</TextHeader1>
  <TextBody1>
    Helo Lang is my toy language that I built out of interest.
    It draws inspiration from Rust and Ocaml.
    Of course, as a toy, it can be packed full of bugs, not to mention its not-user-friendly error messages.
    Most likely, even myself won't apply it in any pratical applications.
  </TextBody1>

  <TextHeader2>Features</TextHeader2>
  <TextBody1>
    Helo Lang was originall built to be a strict-evaluated functional language.
    To support IO, I took the shortest path of introducing impure functions called routines,
    compared with Haskell's way, since I was discouraged by the complexity of a type system that supports
    higher ranked types. Also, currying is not supported, as I belive implementing only partial application
    is much easier to implement.
  </TextBody1>
  <TextBody1>
    Following list highlights major features of Helo.
  </TextBody1>
  <TextBody1>
    <ul>
      <li>Compile-time type system, with generics and global type inference supported</li>
      <li>Ad-hoc polymorphism that can be user-defined through something like traits of Rust</li>
      <li>Primitive types include Int, Float, Bool and Str. Operations on them are provided.</li>
      <li>Algebraic data types, and pattern matching on them</li>
      <li>Module system to organize code into many files</li>
      <li>Partial application</li>
      <li>Closure. both anonymous closures and recursives closures are supported.</li>
      <li>Mutability of local variables and IO in an impure function</li>
      <li>
        Simple compile-time optimizations including inlining, dead code elimination, constant propagation, common expression elimination,
        control flow graph simplication and tail-call optimiztion
      </li>
      <li>Compiles to a kind of byte-code executed by the Helo VM</li>
      <li>Mark-and-sweep garbage collector</li>
    </ul>
  </TextBody1>

  <TextHeader2>Hello World</TextHeader2>
  <TextBody1>
    Helo compiler and runtime has been compiled to Web Assembly, so it can run directly in browser,
    just like below.
  </TextBody1>
  <EmbededPlayGround v-model="helloWorld" :height="200" :initial-split="50"></EmbededPlayGround>

  <TextHeader2>Examples</TextHeader2>
  <TextBody1>
    The playground contains some examples of Helo code, for example, an
    <q-btn color="primary" flat dense no-caps to="/playground/AVLTree">AVL Tree</q-btn> and a
    <q-btn color="primary" flat dense no-caps to="/playground/Calculator">Calculator</q-btn>.
  </TextBody1>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import EmbededPlayGround from 'src/components/EmbededPlayGround.vue'
import TextHeader1 from 'src/components/TextHeader1.vue'
import TextHeader2 from 'src/components/TextHeader2.vue'
import TextBody1 from 'src/components/TextBody1.vue'

const helloWorld = ref('routine main = io.println "Hello World!"\n')
</script>
