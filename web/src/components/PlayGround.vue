<template>
  <q-splitter v-model="splitterModel" :horizontal="props.horizontal">

    <template v-slot:before>
      <q-tabs class="bg-white" style="height: 50px;" align="left">
        <q-item-label style="margin-left: 15px;">{{ props.name }}</q-item-label>
        <q-space/>
        <q-btn flat color="primary" label="Run" @click="compileAndRunCode" />
      </q-tabs>
      <v-ace-editor v-model:value="code" lang="txt"
        :style="{ height: props.pageHeight - 50 + 'px', fontSize: '1em' }" :print-margin="false" />
    </template>

    <template v-slot:after>

      <q-tabs v-model="outputTab" class="text-grey" active-color="primary" align="left" style="height: 50px">
        <q-tab name="compile" label="Compile"></q-tab>
        <q-tab name="run" label="run"></q-tab>
        <q-select :multiple="true" v-model="selectedPrintStages" :options="stages.map((_, i) => i)"
          :option-label="i => stages[i]"
          :display-value="selectedPrintStages.length > 0 ? stages[selectedPrintStages[0]] + '...' : 'Select intermediate result print'"
          style="margin-left: 20px;"></q-select>
        <q-btn color="primary" flat label="Clear" @click="clearOutput"/>
      </q-tabs>

      <q-tab-panels v-model="outputTab" :keep-alive="true">
        <q-tab-panel name="compile">
          <q-scroll-area :style="{ height: props.pageHeight - 85 + 'px' }" ref="compileOutputScroll">
            <pre ref="compileOutput" class="code-area"></pre>
          </q-scroll-area>
        </q-tab-panel>

        <q-tab-panel name="run">
          <q-scroll-area :style="{ height: props.pageHeight - 85 + 'px' }" ref="runOutputScroll">
            <pre ref="runOutput" class="code-area"></pre>
            <q-input v-if="vm !== undefined" autogrow dense v-model="input" @keyup.enter="submitInput"
              placeholder="Stdin to VM"></q-input>
          </q-scroll-area>
        </q-tab-panel>

      </q-tab-panels>
    </template>

  </q-splitter>
</template>

<script setup lang="ts">
import { defineProps, onMounted, ref } from 'vue'
import { QScrollArea } from 'quasar'
import { VAceEditor } from 'vue3-ace-editor'
// eslint-disable-next-line camelcase
import { Compiler, Stage, Src, JsOutput, Vm, JsIo, ExecutionResultType, register_panic_hook } from 'src/wasm/helo_wasm'

const props = defineProps<{
  pageHeight: number,
  name?: string
  horizontal?: boolean
}>()

onMounted(() => {
  register_panic_hook()
})

const code = defineModel<string>({ required: true })

const splitterModel = ref(50)
const stages = Object.keys(Stage).filter(item => isNaN(Number(item)))
const outputTab = ref('compile')
const selectedPrintStages = ref([Stage.ByteCode])
const compileOutput = ref<HTMLInputElement | null>(null)
const runOutput = ref<HTMLInputElement | null>(null)
const compileOutputScroll = ref<QScrollArea | null>(null)
const runOutputScroll = ref<QScrollArea | null>(null)
const input = ref('')

let compileOutputContent = ''
let runOutputContent = ''

function clearOutput () {
  const outputElem = compileOutput?.value
  if (outputElem !== null) {
    outputElem.innerText = ''
  }
  const outputElem1 = runOutput?.value
  if (outputElem1 !== null) {
    outputElem1.innerText = ''
  }
  runOutputContent = ''
  compileOutputContent = ''
}

function writeCompileOutput (s: string) {
  compileOutputContent += s
}

function flushCompileOutput () {
  const outputElem = compileOutput?.value
  if (outputElem !== null) {
    outputElem.innerText = compileOutputContent
  }
  setTimeout(() => {
    if (compileOutputScroll?.value !== null) {
      compileOutputScroll.value.setScrollPercentage('vertical', 100)
    }
  }, 10)
}

function writeRunOutput (s: string) {
  runOutputContent += s
}

function flushRunOutput () {
  const outputElem = runOutput?.value
  if (outputElem !== null) {
    outputElem.innerText = runOutputContent
  }
  setTimeout(() => {
    if (runOutputScroll?.value !== null) {
      runOutputScroll.value.setScrollPercentage('vertical', 100)
    }
  }, 10)
}

function calcRowWidth (): number {
  // Create a hidden span element to measure character width
  const span = document.createElement('span')
  span.innerText = 'A' // Any character can be used here
  span.style.fontFamily = 'monospace'
  span.style.fontSize = '14px'
  span.style.visibility = 'hidden'
  document.body.appendChild(span)

  const charWidth = span.offsetWidth

  document.body.removeChild(span)

  const containerWidth = compileOutput?.value?.offsetWidth

  if (containerWidth !== undefined) {
    const outputWidth = Math.floor(containerWidth / charWidth)
    console.log(`output width is ${outputWidth}`)
    return outputWidth
  } else {
    return 80
  }
}

const vm = ref<Vm | undefined>(undefined)
let src: Src | undefined

function compileAndRunCode () {
  src = Src.new()
  src.insert([], 'main', code.value)
  src.set_root_file_name('main')

  const jsOutput = JsOutput.new(writeCompileOutput, flushCompileOutput)

  const compiler = Compiler.new(selectedPrintStages.value, true, calcRowWidth())
  const artifect = compiler.compile(jsOutput, src.clone())
  flushCompileOutput()

  if (artifect !== undefined) {
    const vmNew = Vm.new(artifect)
    const jsIo = JsIo.new(writeRunOutput, flushRunOutput, '')
    runVm(vmNew, jsIo)
  }
}

function runVm (vmNew: Vm, io: JsIo) {
  writeRunOutput('------ Begin of Execution ------\n')
  flushRunOutput()

  setTimeout(() => { outputTab.value = 'run' }, 100)
  const result = vmNew.run(io)

  if (result.type_() === ExecutionResultType.Exception || result.type_() === ExecutionResultType.Returned) {
    const r = result.unwrap_str()
    writeRunOutput('------ End of Execution ------\n')
    writeRunOutput('Program returned ' + r + '\n')
    vm.value = undefined
  } else if (result.type_() === ExecutionResultType.Panic) {
    const jsOutput = JsOutput.new(writeRunOutput, flushRunOutput)
    if (src === undefined) {
      console.error('src is undefined when attempting to use it to print panic')
      return
    }
    result.unwrap_panic().print_to(jsOutput, src)
    vm.value = undefined
  } else if (result.type_() === ExecutionResultType.Hung) {
    console.log('vm hung')
    vm.value = result.unwrap_vm()
  }
  setTimeout(() => flushRunOutput(), 10)
}

function submitInput () {
  if (vm.value !== undefined) {
    const jsIo = JsIo.new(writeRunOutput, flushRunOutput, input.value)
    writeRunOutput(input.value)
    flushRunOutput()
    setTimeout(() => {
      input.value = ''
    }, 10)
    runVm(vm.value, jsIo)
  }
}

</script>

<style>
.code-area {
  font-family: "monospace"
}
</style>
