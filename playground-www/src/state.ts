import type { UnknownRecord } from '../../src/interface'

export const defaultState = {
  'playground-height': 350 as number,
  'resize-divider-1-percent': 20 as number,
  'resize-divider-2-percent': 60 as number,
  'context-trash-bin': '' as string,
  'context': '' as string,
  'context-scroll-top': 0 as number,
  'lits-code-trash-bin': '' as string,
  'lits-code': '' as string,
  'lits-code-scroll-top': 0 as number,
  'output-trash-bin': '' as string,
  'output': '' as string,
  'output-scroll-top': 0 as number,
} as const

type State = {
  -readonly [K in keyof typeof defaultState]: typeof defaultState[K]
}

type Key = keyof typeof defaultState
type StorageKey = `playground-${Key}`

const keys = Object.keys(defaultState) as Key[]

export const state: State = {
  ...defaultState,
}

function getStorageKey(key: Key): StorageKey {
  return `playground-${key}`
}

keys.forEach((key: Key) => {
  const value = localStorage.getItem(getStorageKey(key))

  ;(state as UnknownRecord)[key] = typeof value === 'string' ? JSON.parse(value) : defaultState[key]
})

export function saveState<T extends keyof State>(key: T, value: State[T]) {
  state[key] = value
  localStorage.setItem(getStorageKey(key), JSON.stringify(value))
}

export function clearAllStates() {
  localStorage.clear()
  Object.assign(state, defaultState)
}

export function clearState(key: Key) {
  localStorage.removeItem(getStorageKey(key))
  ;(state as UnknownRecord)[key] = defaultState[key]
}

export function getState<T extends keyof State>(key: T): State[T] {
  return state[key]
}

export function encodeState() {
  const sharedState: Partial<State> = {
    'lits-code': state['lits-code'],
    'context': state.context,
  }
  return btoa(JSON.stringify(sharedState))
}

export function decodeState(encodedState: string) {
  try {
    const decodedState = JSON.parse(atob(encodedState)) as Partial<State>
    Object.entries(decodedState).forEach(([key, value]) => {
      if (keys.includes(key as Key))
        saveState(key as Key, value)
    })
  }
  catch (error) {
    console.error('Invalid state', encodedState)
    throw error
  }
}
