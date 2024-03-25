<template>
  <router-view />
</template>

<script setup lang="ts">
import { onMounted } from 'vue'
// eslint-disable-next-line camelcase
import { register_panic_hook } from './wasm/helo_wasm'
defineOptions({
  name: 'App'
})

onMounted(() => {
  register_panic_hook()
})
</script>
