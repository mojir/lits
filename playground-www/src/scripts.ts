/* eslint-disable no-console */
import { stringifyValue, throttle } from '../../common/utils'
import type { Example } from '../../reference/examples'
import type { LitsParams } from '../../src'
import { Lits } from '../../src'
import { asUnknownRecord } from '../../src/typeGuards'
import { Search } from './Search'
import { decodeState as applyEncodedState, clearAllStates, clearState, encodeState, getState, saveState } from './state'

const lits = new Lits({ debug: true })
const litsNoDebug = new Lits({ debug: false })

const elements = {
  wrapper: document.getElementById('wrapper') as HTMLElement,
  playground: document.getElementById('playground') as HTMLElement,
  sidebar: document.getElementById('sidebar') as HTMLElement,
  mainPanel: document.getElementById('main-panel') as HTMLElement,
  contextPanel: document.getElementById('context-panel') as HTMLElement,
  litsPanel: document.getElementById('lits-panel') as HTMLElement,
  outputPanel: document.getElementById('output-panel') as HTMLElement,
  moreMenu: document.getElementById('more-menu') as HTMLElement,
  contextTextArea: document.getElementById('context-textarea') as HTMLTextAreaElement,
  outputResult: document.getElementById('output-result') as HTMLElement,
  litsTextArea: document.getElementById('lits-textarea') as HTMLTextAreaElement,
  resizePlayground: document.getElementById('resize-playground') as HTMLElement,
  resizeDevider1: document.getElementById('resize-divider-1') as HTMLElement,
  resizeDevider2: document.getElementById('resize-divider-2') as HTMLElement,
}

type MoveParams = {
  id: 'playground'
  startMoveY: number
  heightBeforeMove: number
} | {
  id: 'resize-divider-1' | 'resize-divider-2'
  startMoveX: number
  percentBeforeMove: number
}

type OutputType = 'error' | 'output' | 'result' | 'analyze' | 'tokenize' | 'parse' | 'comment'

let moveParams: MoveParams | null = null

function calculateDimensions() {
  return {
    windowHeight: window.innerHeight,
    windowWidth: window.innerWidth,
  }
}

export function toggleMoreMenu() {
  elements.moreMenu.style.display = elements.moreMenu.style.display === 'block' ? 'none' : 'block'
}

export function closeMoreMenu() {
  elements.moreMenu.style.display = 'none'
}

export function share() {
  addOutputSeparator()
  appendOutput('Sharable link:', 'comment')
  const href = `${location.origin}${location.pathname}?state=${encodeState()}`
  const a = document.createElement('a')
  a.textContent = href
  a.className = 'share-link'
  a.href = href
  addOutputElement(a)
}

function onDocumentClick(event: Event) {
  const target = event.target as HTMLInputElement | undefined
  if (target?.closest('#more-menu'))
    return

  if (elements.moreMenu.style.display === 'block') {
    event.stopPropagation()
    closeMoreMenu()
  }
}

function layout() {
  const { windowWidth } = calculateDimensions()

  const playgroundHeight = getState('playground-height')

  const contextPanelWidth = (windowWidth * getState('resize-divider-1-percent')) / 100
  const outputPanelWidth = (windowWidth * (100 - getState('resize-divider-2-percent'))) / 100
  const litsPanelWidth = windowWidth - contextPanelWidth - outputPanelWidth

  elements.playground.style.height = `${playgroundHeight}px`
  elements.contextPanel.style.width = `${contextPanelWidth}px`
  elements.litsPanel.style.width = `${litsPanelWidth}px`
  elements.outputPanel.style.width = `${outputPanelWidth}px`
  elements.sidebar.style.bottom = `${playgroundHeight}px`
  elements.mainPanel.style.bottom = `${playgroundHeight}px`
  elements.wrapper.style.display = 'block'
}

export function resetPlayground() {
  clearAllStates()

  resetContext()
  resetLitsCode()
  resetOutput()
  Search.closeSearch()
  Search.clearSearch()

  layout()
}

export function resetContext() {
  const context = getState('context')
  if (context === '') {
    setContext(getState('context-trash-bin'))
  }
  else {
    saveState('context-trash-bin', context)
    elements.contextTextArea.value = ''
    clearState('context')
  }
}

function setContext(value: string) {
  elements.contextTextArea.value = value
  elements.contextTextArea.scrollTop = 0
  saveState('context', value)
}

function getParsedContext(): Record<string, unknown> {
  try {
    return asUnknownRecord(JSON.parse(getState('context')))
  }
  catch (e) {
    return {}
  }
}

export function addParam() {
  const context = getParsedContext()
  const values = {
    n: 42,
    s: 'foo bar',
    arr: ['foo', 'bar', 1, 2, true, false, null],
    obj: {
      name: 'John Doe',
      age: 42,
      married: true,
      children: ['Alice', 'Bob'],
      address: {
        street: '123 Main St',
        city: 'Springfield',
        state: 'IL',
        zip: '62701',
      },
    },
  }

  context.values = Object.assign(values, context.values)

  setContext(JSON.stringify(context, null, 2))
}

export function resetLitsCode(force = false) {
  const litsCode = getState('lits-code')
  if (litsCode === '' && !force) {
    setLitsCode(getState('lits-code-trash-bin'), 'top')
  }
  else {
    if (force)
      saveState('lits-code-trash-bin', '')
    else
      saveState('lits-code-trash-bin', litsCode)

    elements.litsTextArea.value = ''
    clearState('lits-code')
  }
}

function setLitsCode(value: string, scroll?: 'top' | 'bottom') {
  elements.litsTextArea.value = value
  saveState('lits-code', value)
  if (scroll === 'top')
    elements.litsTextArea.scrollTo(0, 0)
  else if (scroll === 'bottom')
    elements.litsTextArea.scrollTo({ top: elements.litsTextArea.scrollHeight, behavior: 'smooth' })
}

function appendLitsCode(value: string) {
  const oldContent = getState('lits-code').trimEnd()

  const newContent = oldContent ? `${oldContent}\n\n${value}` : value.trim()
  setLitsCode(newContent, 'bottom')
}

export function resetOutput(force = false) {
  const output = getState('output')
  if (output === '' && !force) {
    const trash = getState('output-trash-bin')
    elements.outputResult.innerHTML = trash
    saveState('output', trash)

    elements.outputResult.scrollTop = elements.outputResult.scrollHeight
  }
  else {
    if (force)
      saveState('output-trash-bin', '')
    else
      saveState('output-trash-bin', output)

    elements.outputResult.innerHTML = ''
    clearState('output')
  }
}

function hasOutput() {
  return getState('output').trim() !== ''
}

function setOutput(value: string) {
  elements.outputResult.innerHTML = value
  saveState('output', value)
}

function appendOutput(output: unknown, className: OutputType) {
  const outputElement = document.createElement('span')
  outputElement.className = className
  outputElement.textContent = `${output}`
  addOutputElement(outputElement)
}

function addOutputSeparator() {
  if (hasOutput()) {
    const separator = document.createElement('div')
    separator.className = 'separator'
    addOutputElement(separator)
  }
}

function addOutputElement(element: HTMLElement) {
  elements.outputResult.appendChild(element)
  elements.outputResult.scrollTop = elements.outputResult.scrollHeight

  saveState('output', elements.outputResult.innerHTML)
}

window.onload = function () {
  document.addEventListener('click', onDocumentClick, true)

  elements.resizePlayground.onmousedown = (event) => {
    moveParams = {
      id: 'playground',
      startMoveY: event.clientY,
      heightBeforeMove: getState('playground-height'),
    }
  }

  elements.resizeDevider1.onmousedown = (event) => {
    moveParams = {
      id: 'resize-divider-1',
      startMoveX: event.clientX,
      percentBeforeMove: getState('resize-divider-1-percent'),
    }
  }

  elements.resizeDevider2.onmousedown = (event) => {
    moveParams = {
      id: 'resize-divider-2',
      startMoveX: event.clientX,
      percentBeforeMove: getState('resize-divider-2-percent'),
    }
  }

  window.onresize = throttle(layout)
  window.onmouseup = () => {
    document.body.classList.remove('no-select')
    moveParams = null
  }

  window.onmousemove = throttle((event: MouseEvent) => {
    const { windowHeight, windowWidth } = calculateDimensions()
    if (moveParams === null)
      return

    document.body.classList.add('no-select')

    if (moveParams.id === 'playground') {
      let playgroundHeight = moveParams.heightBeforeMove + moveParams.startMoveY - event.clientY
      if (playgroundHeight < 30)
        playgroundHeight = 30

      if (playgroundHeight > windowHeight)
        playgroundHeight = windowHeight

      saveState('playground-height', playgroundHeight)
    }
    else if (moveParams.id === 'resize-divider-1') {
      let resizeDivider1XPercent
        = moveParams.percentBeforeMove + ((event.clientX - moveParams.startMoveX) / windowWidth) * 100
      if (resizeDivider1XPercent < 10)
        resizeDivider1XPercent = 10

      if (resizeDivider1XPercent > getState('resize-divider-2-percent') - 10)
        resizeDivider1XPercent = getState('resize-divider-2-percent') - 10

      saveState('resize-divider-1-percent', resizeDivider1XPercent)
    }
    else if (moveParams.id === 'resize-divider-2') {
      let resizeDivider2XPercent
        = moveParams.percentBeforeMove + ((event.clientX - moveParams.startMoveX) / windowWidth) * 100
      if (resizeDivider2XPercent < getState('resize-divider-1-percent') + 10)
        resizeDivider2XPercent = getState('resize-divider-1-percent') + 10

      if (resizeDivider2XPercent > 90)
        resizeDivider2XPercent = 90

      saveState('resize-divider-2-percent', resizeDivider2XPercent)
    }
    layout()
  })

  window.addEventListener('keydown', (evt) => {
    if (Search.handleKeyDown(evt))
      return

    if (evt.key === 'F5') {
      evt.preventDefault()
      run()
    }
    if (evt.key === 'Escape') {
      closeMoreMenu()
      evt.preventDefault()
    }
  })
  elements.contextTextArea.addEventListener('keydown', keydownHandler)
  elements.contextTextArea.addEventListener('input', (event: Event) => {
    const target = event.target as HTMLInputElement | undefined
    if (target)
      setContext(target.value)
  })
  elements.contextTextArea.addEventListener('scroll', () => {
    saveState('context-scroll-top', elements.contextTextArea.scrollTop)
  })

  elements.litsTextArea.addEventListener('keydown', keydownHandler)
  elements.litsTextArea.addEventListener('input', (event: Event) => {
    const target = event.target as HTMLInputElement | undefined
    if (target)
      setLitsCode(target.value)
  })
  elements.litsTextArea.addEventListener('scroll', () => {
    saveState('lits-code-scroll-top', elements.litsTextArea.scrollTop)
  })

  elements.outputResult.addEventListener('scroll', () => {
    saveState('output-scroll-top', elements.outputResult.scrollTop)
  })

  const urlParams = new URLSearchParams(window.location.search)

  const urlState = urlParams.get('state')
  if (urlState) {
    applyEncodedState(urlState)
    urlParams.delete('state')
    history.replaceState(null, '', `${location.pathname}${urlParams.toString() ? '?' : ''}${urlParams.toString()}`)
  }

  setContext(getState('context'))
  setLitsCode(getState('lits-code'), 'top')
  setOutput(getState('output'))

  setTimeout(() => {
    elements.contextTextArea.scrollTop = getState('context-scroll-top')
    elements.litsTextArea.scrollTop = getState('lits-code-scroll-top')
    elements.outputResult.scrollTop = getState('output-scroll-top')
  }, 0)

  const id = location.hash.substring(1) || 'index'
  showPage(id, 'instant', 'replace')

  layout()
}

function keydownHandler(evt: KeyboardEvent) {
  if (evt.key === 'Enter' && evt.ctrlKey) {
    evt.preventDefault()
    const target = evt.target as HTMLTextAreaElement
    const { selectionStart, selectionEnd } = target
    if (selectionEnd > selectionStart) {
      const program = target.value.substring(selectionStart, selectionEnd)
      run(program)
    }
    else {
      run()
    }
    return
  }

  if (['Tab', 'Backspace', 'Enter', 'Delete'].includes(evt.key)) {
    const target = evt.target as HTMLTextAreaElement
    const start = target.selectionStart
    const end = target.selectionEnd

    const indexOfReturn = target.value.lastIndexOf('\n', start - 1)
    const rowLength = start - indexOfReturn - 1
    const onTabStop = rowLength % 2 === 0
    if (evt.key === 'Tab') {
      evt.preventDefault()
      if (!evt.shiftKey) {
        target.value = target.value.substring(0, start) + (onTabStop ? '  ' : ' ') + target.value.substring(end)
        target.selectionStart = target.selectionEnd = start + (onTabStop ? 2 : 1)
      }
    }
    if (evt.key === 'Backspace') {
      if (onTabStop && start === end && target.value.substr(start - 2, 2) === '  ') {
        evt.preventDefault()
        target.value = target.value.substring(0, start - 2) + target.value.substring(end)
        target.selectionStart = target.selectionEnd = start - 2
      }
    }
    if (evt.key === 'Enter') {
      evt.preventDefault()
      const spaceCount = target.value.substring(indexOfReturn + 1, start).replace(/^( *).*/, '$1').length
      target.value = `${target.value.substring(0, start)}\n${' '.repeat(spaceCount)}${target.value.substring(end)}`
      target.selectionStart = target.selectionEnd = start + 1 + spaceCount
    }
    if (evt.key === 'Delete') {
      if (onTabStop && start === end && target.value.substr(start, 2) === '  ') {
        evt.preventDefault()
        target.value = target.value.substring(0, start) + target.value.substring(end + 2)
        target.selectionStart = target.selectionEnd = start
      }
    }
  }
}

window.addEventListener('popstate', () => {
  const id = location.hash.substring(1) || 'index'
  showPage(id, 'instant', 'none')
})

function truncateCode(text: string, count = 1000) {
  const oneLiner = text
    .split('\n')
    .map(line => line.trim())
    .filter(line => line.length > 0)
    .filter(line => !line.startsWith(';'))
    .join(' ')
  if (oneLiner.length <= count)
    return oneLiner
  else
    return `${oneLiner.substring(0, count - 3)}...`
}
export function run(program?: string) {
  addOutputSeparator()
  const code = program || getState('lits-code')

  if (program)
    appendOutput(`Run selection: ${truncateCode(code)}`, 'comment')
  else
    appendOutput(`Run: ${truncateCode(code)}`, 'comment')

  const contextString = getState('context')
  let context: LitsParams
  try {
    context
      = contextString.trim().length > 0
        ? JSON.parse(contextString, (_, val) =>
          // eslint-disable-next-line no-eval, ts/no-unsafe-return
          typeof val === 'string' && val.startsWith('EVAL:') ? eval(val.substring(5)) : val) as LitsParams
        : {}
  }
  catch {
    appendOutput(`Error: Could not parse context: ${contextString}`, 'error')
    return
  }
  let result
  const oldLog = console.log
  console.log = function (...args) {
    const logRow = args.map(arg => stringifyValue(arg, false)).join(' ')
    appendOutput(logRow, 'output')
  }
  const oldWarn = console.warn
  console.warn = function (...args: unknown[]) {
    oldWarn.apply(console, args)
    appendOutput(args[0], 'output')
  }
  try {
    result = lits.run(code, context)
  }
  catch (error) {
    appendOutput(error, 'error')
    return
  }
  finally {
    console.log = oldLog
    console.warn = oldWarn
  }
  const content = stringifyValue(result, false)

  appendOutput(content, 'result')
}

export function analyze() {
  addOutputSeparator()
  const code = getState('lits-code')
  appendOutput(`Analyze: ${truncateCode(code)}`, 'comment')
  let result
  const oldLog = console.log
  console.log = function (...args) {
    const logRow = args.map(arg => stringifyValue(arg, false)).join(' ')
    appendOutput(logRow, 'output')
  }
  const oldWarn = console.warn
  console.warn = function (...args: unknown[]) {
    oldWarn.apply(console, args)
    appendOutput(args[0], 'output')
  }
  try {
    result = lits.analyze(code)
  }
  catch (error) {
    appendOutput(error, 'error')
    return
  }
  finally {
    console.log = oldLog
    console.warn = oldWarn
  }
  const undefinedSymbols = [...result.undefinedSymbols].map(s => s.symbol).join(', ')
  const content = undefinedSymbols
    ? `Undefined symbols: ${undefinedSymbols}`
    : 'No undefined symbols'

  appendOutput(content, 'analyze')
}

export function parse() {
  addOutputSeparator()
  const code = getState('lits-code')
  appendOutput(`Parse: ${truncateCode(code)}`, 'comment')
  let result
  const oldLog = console.log
  console.log = function (...args) {
    const logRow = args.map(arg => stringifyValue(arg, false)).join(' ')
    appendOutput(logRow, 'output')
  }
  const oldWarn = console.warn
  console.warn = function (...args) {
    oldWarn.apply(console, args)
    appendOutput(args[0], 'output')
  }
  try {
    const tokens = litsNoDebug.tokenize(code)
    result = litsNoDebug.parse(tokens)
  }
  catch (error) {
    appendOutput(error, 'error')
    return
  }
  finally {
    console.log = oldLog
    console.warn = oldWarn
  }
  const content = JSON.stringify(result, null, 2)

  appendOutput(content, 'parse')
}

export function tokenize() {
  addOutputSeparator()
  const code = getState('lits-code')
  appendOutput(`Tokenize: ${truncateCode(code)}`, 'comment')

  let result
  const oldLog = console.log
  console.log = function (...args: unknown[]) {
    const logRow = args.map(arg => stringifyValue(arg, false)).join(' ')
    appendOutput(logRow, 'output')
  }
  const oldWarn = console.warn
  console.warn = function (...args: unknown[]) {
    oldWarn.apply(console, args)
    appendOutput(args[0], 'output')
  }
  try {
    result = litsNoDebug.tokenize(code)
  }
  catch (error) {
    appendOutput(error, 'error')
    return
  }
  finally {
    console.log = oldLog
    console.warn = oldWarn
  }
  const content = JSON.stringify(result, null, 2)

  appendOutput(content, 'tokenize')
}

export function showPage(id: string, scroll: 'smooth' | 'instant' | 'none', historyEvent: 'replace' | 'push' | 'none' = 'push') {
  setTimeout(() => {
    inactivateAll()

    Search.closeSearch()
    const page = document.getElementById(id)
    const linkElementId = `${(!id || id === 'index') ? 'home-page' : id}_link`
    const link = document.getElementById(linkElementId)

    if (!id || id === 'index' || id === 'example-page')
      elements.mainPanel.scrollTo({ top: 0 })

    if (!page) {
      showPage('index', scroll, 'replace')
      return
    }

    page.classList.add('active-content')
    if (link) {
      link.classList.add('active-sidebar-entry')
      if (scroll !== 'none')
        link.scrollIntoView({ block: 'center', behavior: scroll })
    }

    if (id === 'index')
      history.replaceState(null, 'Lits', window.location.pathname + window.location.search)

    else if (historyEvent === 'replace')
      history.replaceState(null, '', `#${id}`)

    else if (historyEvent !== 'none')
      history.pushState(null, '', `#${id}`)
  }, 0)
}

function inactivateAll() {
  let els = document.getElementsByClassName('active-content')
  while (els[0])
    els[0].classList.remove('active-content')

  els = document.getElementsByClassName('active-sidebar-entry')
  while (els[0])
    els[0].classList.remove('active-sidebar-entry')
}

export function addToPlayground(name: string, encodedExample: string) {
  const example = atob(encodedExample)
  appendLitsCode(`;; Example - ${name} ;;\n\n${example}\n`)
}

export function setPlayground(name: string, encodedExample: string) {
  const example = JSON.parse(atob(encodedExample)) as Example

  const context = example.context
    // eslint-disable-next-line ts/no-unsafe-return
    ? JSON.stringify(example.context, (_k, v) => (v === undefined ? null : v), 2)
    : ''

  setContext(context)

  const code = example.code ? example.code : ''
  const size = Math.max(name.length + 10, 40)
  const paddingLeft = Math.floor((size - name.length) / 2)
  const paddingRight = Math.ceil((size - name.length) / 2)
  setLitsCode(`
${`;;${'-'.repeat(size)};;`}
${`;;${' '.repeat(paddingLeft)}${name}${' '.repeat(paddingRight)};;`}
${`;;${'-'.repeat(size)};;`}

${code}
`.trimStart(), 'top')
}
