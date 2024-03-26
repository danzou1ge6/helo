import { RouteRecordRaw } from 'vue-router'

const routes: RouteRecordRaw[] = [
  {
    path: '/',
    redirect: '/ref/introduction'
  },
  {
    path: '/playground/:exampleName',
    component: () => import('layouts/EditorLayout.vue'),
    props: true
  },
  {
    path: '/playground',
    component: () => import('layouts/EditorLayout.vue')
  },
  {
    path: '/doc',
    component: () => import('layouts/DocumentLayout.vue'),
    children: [
      {
        path: 'introduction',
        component: () => import('pages/RefIntroduction.vue')
      },
      {
        path: 'basics',
        component: () => import('pages/RefBasics.vue')
      },
      {
        path: 'closures',
        component: () => import('pages/RefClosures.vue')
      },
      {
        path: 'relation',
        component: () => import('pages/RefRelation.vue')
      },
      {
        path: 'module',
        component: () => import('pages/RefModule.vue')
      },
      {
        path: 'architecture',
        component: () => import('pages/RefArchitecture.vue')
      }
    ]
  },
  // Always leave this as last one,
  // but you can also remove it
  {
    path: '/:catchAll(.*)*',
    component: () => import('pages/ErrorNotFound.vue')
  }
]

export default routes
