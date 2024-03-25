<template>
  <q-layout view="hHh Lpr lFf">
    <q-header style="height: 50px">
      <q-toolbar>
        <q-btn flat dense icon="menu" aria-label="Menu" @click="toggleLeftDrawer" />

        <q-toolbar-title style="">
          Helo
        </q-toolbar-title>

        <NavigationButtons></NavigationButtons>
      </q-toolbar>
    </q-header>

    <q-drawer v-model="leftDrawerOpen" bordered>
      <q-item-label header>Documentation</q-item-label>
      <q-list>
        <q-item clickable v-ripple v-for="entry in entries" :key="entry.name" :to="'/doc/' + entry.path">
          <q-item-section>{{ entry.name }}</q-item-section>
        </q-item>
      </q-list>
    </q-drawer>

    <q-page-container>
      <div class="q-pa-sm">
        <q-card flat :bordered="false">
          <q-card-section class="q-ma-md">
            <router-view></router-view>
          </q-card-section>
        </q-card>
      </div>
    </q-page-container>
  </q-layout>
</template>
<script setup lang="ts">
import { ref } from 'vue'
import NavigationButtons from 'src/components/NavigationButtons.vue'

const leftDrawerOpen = ref(true)

function toggleLeftDrawer () {
  leftDrawerOpen.value = !leftDrawerOpen.value
}

const entries = ref([
  { name: 'Introduction', path: 'introduction' },
  { name: 'Basics', path: 'basics' },
  { name: 'Closures', path: 'closures' },
  { name: 'Relation', path: 'relation' },
  { name: 'Module', path: 'module' },
  { name: 'Architecture', path: 'architecture' }
])

</script>
