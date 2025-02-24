import type { UnknownRecord } from '../../src/interface'
import type { HistoryEntry, HistoryStatus } from './StateHistory'
import { StateHistory } from './StateHistory'

export const defaultState = {
  'playground-height': 350 as number,
  'resize-divider-1-percent': 20 as number,
  'resize-divider-2-percent': 60 as number,
  'context': '' as string,
  'context-scroll-top': 0 as number,
  'context-selection-start': 0 as number,
  'context-selection-end': 0 as number,
  'lits-code': '' as string,
  'lits-code-scroll-top': 0 as number,
  'lits-code-selection-start': 0 as number,
  'lits-code-selection-end': 0 as number,
  'output': '' as string,
  'output-scroll-top': 0 as number,
  'new-context-name': '' as string,
  'new-context-value': '' as string,
  'debug': false as boolean,
  'algebraic': false as boolean,
  'focused-panel': null as 'lits-code' | 'context' | null,
} as const

type State = {
  -readonly [K in keyof typeof defaultState]: typeof defaultState[K]
}

type Key = keyof typeof defaultState
type StorageKey = `playground-${Key}`

let contextHistoryListener: undefined | ((status: HistoryStatus) => void)
let litsCodeHistoryListener: undefined | ((status: HistoryStatus) => void)

const state: State = {
  ...defaultState,
}

;(Object.keys(defaultState) as Key[]).forEach((key: Key) => {
  const value = localStorage.getItem(getStorageKey(key))

  ;(state as UnknownRecord)[key] = typeof value === 'string' ? JSON.parse(value) : defaultState[key]
})

const contextHistory = new StateHistory(createContextHistoryEntry(), (status) => {
  contextHistoryListener?.(status)
})

const litsCodeHistory = new StateHistory(createLitsCodeHistoryEntry(), (status) => {
  litsCodeHistoryListener?.(status)
})

function createContextHistoryEntry(): HistoryEntry {
  return {
    text: state.context,
    selectionStart: state['context-selection-start'],
    selectionEnd: state['context-selection-end'],
  }
}

function createLitsCodeHistoryEntry(): HistoryEntry {
  return {
    text: state['lits-code'],
    selectionStart: state['lits-code-selection-start'],
    selectionEnd: state['lits-code-selection-end'],
  }
}

function pushHistory() {
  contextHistory.push(createContextHistoryEntry())
  litsCodeHistory.push(createLitsCodeHistoryEntry())
}

export function setContextHistoryListener(listener: (status: HistoryStatus) => void) {
  contextHistoryListener = listener
}

export function setLitsCodeHistoryListener(listener: (status: HistoryStatus) => void) {
  litsCodeHistoryListener = listener
}

export function saveState(newState: Partial<State>, pushToHistory = true) {
  Object.entries(newState).forEach((entry) => {
    const key = entry[0] as keyof State
    const value = entry[1]
    setState(key, value)
    localStorage.setItem(getStorageKey(key), JSON.stringify(value))
  })
  if (pushToHistory) {
    pushHistory()
  }
}

function setState<T extends keyof State>(key: T, value: State[T]) {
  state[key] = value
}

export function clearAllStates() {
  localStorage.clear()
  Object.assign(state, defaultState)
  litsCodeHistory.reset(createLitsCodeHistoryEntry())
  contextHistory.reset(createContextHistoryEntry())
}

export function clearState(...keys: Key[]) {
  keys.forEach((key) => {
    localStorage.removeItem(getStorageKey(key))
    ;(state as UnknownRecord)[key] = defaultState[key]
  })
  pushHistory()
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

export function applyEncodedState(encodedState: string): boolean {
  try {
    saveState(JSON.parse(atob(encodedState)) as Partial<State>, true)
    return true
  }
  catch (error) {
    return false
  }
}

export function undoContext() {
  try {
    const historyEntry = contextHistory.undo()
    saveState({
      'context': historyEntry.text,
      'context-selection-start': historyEntry.selectionStart,
      'context-selection-end': historyEntry.selectionEnd,
    }, false)
    return true
  }
  catch {
    return false
  }
}

export function redoContext() {
  try {
    const historyEntry = contextHistory.redo()
    saveState({
      'context': historyEntry.text,
      'context-selection-start': historyEntry.selectionStart,
      'context-selection-end': historyEntry.selectionEnd,
    }, false)
    return true
  }
  catch {
    return false
  }
}

export function undoLitsCode() {
  try {
    const historyEntry = litsCodeHistory.undo()
    saveState({
      'lits-code': historyEntry.text,
      'lits-code-selection-start': historyEntry.selectionStart,
      'lits-code-selection-end': historyEntry.selectionEnd,
    }, false)
    return true
  }
  catch {
    return false
  }
}

export function redoLitsCode() {
  try {
    const historyEntry = litsCodeHistory.redo()
    saveState({
      'lits-code': historyEntry.text,
      'lits-code-selection-start': historyEntry.selectionStart,
      'lits-code-selection-end': historyEntry.selectionEnd,
    }, false)
    return true
  }
  catch {
    return false
  }
}

function getStorageKey(key: Key): StorageKey {
  return `playground-${key}`
}
