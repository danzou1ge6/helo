<template>
  <q-card class="q-ma-md">
    <PlayGround :page-height="height" v-model="code" :show-output="showOutput"
      v-model:split="split"
      :run-on-mount="true" >
      <q-btn flat dense color="primary" @click="showOutput = !showOutput" label="Show Output" no-caps></q-btn>
    </PlayGround>
  </q-card>
</template>

<script setup lang="ts">
import PlayGround from './PlayGround.vue'
import { onMounted, ref } from 'vue'

const split = ref(70)

const code = defineModel<string>({ required: true })
const props = defineProps<{ height: number, initialSplit?: number }>()

onMounted(() => {
  if (props.initialSplit !== undefined) {
    split.value = props.initialSplit
  }
})

const showOutput = ref(true)
</script>
