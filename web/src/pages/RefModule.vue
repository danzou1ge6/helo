<template>
  <TextHeader1>Module</TextHeader1>
  <TextBody1>
    We have a simple module system to aid organizing code.
    What this module system solely does is seperating namespaces, to enable more concise naming.
    Unfortunately, this module system doesn't act as compilation unit,
    nor is higher ranked module supported.
  </TextBody1>
  <TextBody1>
    Each source file can also be seperated into multiple modueles by <TextCode>module</TextCode> keyword.
  </TextBody1>
  <EmbededPlayGround v-model="moduleCode" :height="300"></EmbededPlayGround>

  <TextHeader2>Importing Modules</TextHeader2>
  <TextBody1>
    Modules can be imported using the <TextCode>use</TextCode> keyword.
    There are two variants of <TextCode>use</TextCode> commands.
    <ul>
      <li><TextCode>use module_name</TextCode> imports all symbols from <TextCode>module_name</TextCode></li>
      <li>
        <TextCode>use module_name of f1, f2, f3 as f</TextCode> imports <TextCode>f1, f2</TextCode> from
        <TextCode>module_name</TextCode>, as well as importing <TextCode>f3</TextCode> with the name
        <TextCode>f</TextCode>.
      </li>
    </ul>
    <TextCode>use</TextCode> commands can appear in two places
    <ul>
      <li>
        At top level of a module, that is at same level as a function definition.
        In this case, all expressions in the module, regardless of sequence of appearing,
        are affected by the command.
      </li>
      <li>
        In a <TextCode>use</TextCode> expression, in the form of <TextCode>use module_name in ...</TextCode>.
        In this case, only expressions after <TextCode>in</TextCode> are affected.
      </li>
    </ul>
  </TextBody1>
  <EmbededPlayGround v-model="useCode" :height="400"></EmbededPlayGround>
  <TextBody1>
    We adopt a rather strict attitude towards symbol naming.
    Two symbols with the same imported name are not allowed to coexist in a module,
    so be careful with "dumping in" imports in the form of <TextCode>use module_name</TextCode>.
  </TextBody1>
  <EmbededPlayGround v-model="useConflict" :height="300" :initial-split="30"></EmbededPlayGround>

  <TextHeader2>Modules in File System</TextHeader2>
  <TextBody1>
    Where module system comes most handy is that it allows splitting large code base into smaller files.
    The module tree is almost identical to the file tree that it's built from, except in that a
    <TextCode>mod.helo</TextCode> file is considered in the same module as its parent directory.
  </TextBody1>
  <TextBody1>
    When <TextCode>heloc</TextCode> is fed a file, it checks if the file is called <TextCode>mod.helo</TextCode>.
    If so, it process are siblings to this <TextCode>mod.helo</TextCode>, and put their sub module trees
    under <TextCode>/parent_name</TextCode> where <TextCode>parent_name</TextCode> is name of <TextCode>mod.helo</TextCode>'s
    parent directory. The content of <TextCode>mod.helo</TextCode> is put directly under <TextCode>/parent_name</TextCode>.
  </TextBody1>
  <TextBody1>
    If the name fed to <TextCode>heloc</TextCode> is not called <TextCode>mod.helo</TextCode>, then its stem name
    (Let's call it <TextCode>file_name</TextCode>)
    is used as the root. All sub module trees from files in a directory next to <TextCode>file_name.helo</TextCode>
    with identical name is put under <TextCode>/file_name</TextCode>, and the contents of <TextCode>file_name.helo</TextCode>
    goes directly to <TextCode>/file_name</TextCode>.
  </TextBody1>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import TextHeader1 from 'src/components/TextHeader1.vue'
import TextHeader2 from 'src/components/TextHeader2.vue'
import TextBody1 from 'src/components/TextBody1.vue'
import TextCode from 'src/components/TextCode.vue'
import EmbededPlayGround from 'src/components/EmbededPlayGround.vue'

const moduleCode = ref(
`module a
    fn n = 1

    module a1
        fn n1 = 2
    end
end

routine main =
    use ops in
    let x1 = a.n in
    let x2 = a.a1.n1 in
    io.println (fmt.to_string x1)  * " " * (fmt.to_string x2)
`
)

const useCode = ref(
`module a
    fn n = 1

    module a1
        fn n1 = 2
    end
end

routine main =
    use ops in
    use a in
    use a1 of n1 as n2 in
    let x1 = n in
    let x2 = n2 in
    io.println (fmt.to_string x1)  * " " * (fmt.to_string x2)
`
)

const useConflict = ref(
`
module a
    fn n = 1
end

use a

fn n = 1

routine main = n
`
)

</script>
