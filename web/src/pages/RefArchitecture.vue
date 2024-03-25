<template>
  <TextHeader1>Architecture</TextHeader1>
  <TextBody1>
    This section briefly describes the architecture of Helo compiler and runtime.
    The following graph illustrates the passes for compiling source code to byte code.
  </TextBody1>
  <object :data="archIllust" type="image/svg+xml" class="q-pa-md"></object>
  <TextBody1>
    Then, the compiled byte code is ran by a Register-based virtual machine.
    This is very simple virtual machine that runs a big switch-case loop,
    A mark-and-sweep garbage collector is also part of the virtual machine.
    As few as possible lines of unsafe code is employed in the VM.
    Most of them are introduced to implement garbage collector and objects managed by the GC.
    The VM itself contains no unsafe code.
  </TextBody1>
</template>

<script setup lang="ts">
import TextHeader1 from 'src/components/TextHeader1.vue'
import TextBody1 from 'src/components/TextBody1.vue'
import archIllust from 'src/assets/Arch.svg'
</script>
<!--
digraph {
  node [shape=rectangle]
  edge [labeldistance=4.0]

  src -> tast [label="Parse"]
  tast -> ast [label="Resolve symbols"]
  ast -> typed [label="Infere type"]
  typed -> ir [label="Instantiate generic functions"]
  ir -> ir [label="Inline optimization"]
  ir -> lir
  lir -> ssa
  ssa -> ssa [label="Optimize"]
  ssa -> o_lir
  o_lir -> o_lir [label="Simplify control flow"]
  o_lir -> byte

  src [label="Source Code"]
  tast [label="Textual AST: Almost the source code"]
  ast [label="AST: Locals, captures and globals are resolved"]
  typed [label="Typed AST: Like AST, with types annotated on each node"]
  ir [label="IR: Type is erased, and pattern matching is reduced to simpler operations"]
  lir [label="Linear IR: Instructions in a control flow graph"]
  o_lir [label="Optimized Linear IR"]
  ssa [label="SSA Form: Like LIR, but in static single assignment form"]
  byte [label="Byte Code"]

} -->
