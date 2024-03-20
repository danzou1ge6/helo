<template>
  <q-layout view="hHh Lpr lFf">
    <q-header style="height: 50px">
      <q-toolbar>
        <q-btn flat dense icon="menu" aria-label="Menu" @click="toggleLeftDrawer" />

        <q-toolbar-title style="">
          Helo
        </q-toolbar-title>

      </q-toolbar>
    </q-header>

    <q-drawer v-model="leftDrawerOpen" bordered>
      <q-item-label header>Example Scripts</q-item-label>
      <q-list>
        <q-item clickable v-ripple v-for="example, i in exampleCodes" :key="i"
          :active="i == selectedI" active-class="bg-blue-1">
          <q-item-section @click="selectedI = i">{{ example[0] }}</q-item-section>
        </q-item>
        <q-item clickable v-ripple>
          <q-item-section avatar>
            <q-icon class="text-primary" name="add"/>
          </q-item-section>
          <q-item-section class="text-primary">Add New File</q-item-section>
          <q-menu v-model="showNewFileMenu" style="font-size: 1.2rem" anchor="top right">
            <div class="q-pa-sm">
              <q-input autofocus placeholder="File Name" dense v-model="newFileName"
                @keyup.enter="createNewFile"></q-input>
            </div>
          </q-menu>
        </q-item>
      </q-list>
    </q-drawer>

    <q-page-container>
      <PlayGround :page-height="pageHeight" v-model="exampleCodes[selectedI][1]"
        :name="exampleCodes[selectedI][0]"></PlayGround>
    </q-page-container>
  </q-layout>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import { useQuasar } from 'quasar'
import avlSrc from '../../../helo_scripts/avl.helo?raw'
import calculatorSrc from '../../../helo_scripts/calculator.helo?raw'
import relationSrc from '../../../helo_scripts/relation.helo?raw'
import infixConstructorSrc from '../../../helo_scripts/infix_constructor.helo?raw'
import errorMonadSrc from '../../../helo_scripts/error_monad.helo?raw'
import PlayGround from 'src/components/PlayGround.vue'

const $q = useQuasar()
const pageHeight = $q.screen.height - 50

defineOptions({
  name: 'EditorLayout'
})

const leftDrawerOpen = ref(false)
const exampleCodes = ref([
  ['AVL Tree', avlSrc],
  ['Calculator', calculatorSrc],
  ['Relation Example', relationSrc],
  ['Infix Constructor Example', infixConstructorSrc],
  ['Error Monad Example', errorMonadSrc]
])
const selectedI = ref(0)

function toggleLeftDrawer () {
  leftDrawerOpen.value = !leftDrawerOpen.value
}

const newFileName = ref('')
const showNewFileMenu = ref(false)

function createNewFile () {
  exampleCodes.value.push([newFileName.value, ''])
  showNewFileMenu.value = false
  selectedI.value = exampleCodes.value.length - 1
}

</script>
