/* eslint-disable no-console */
/* eslint-disable no-unused-vars */
/* eslint-disable no-undef */

var defaultProgram = `(defn factorial [x]
  (if (= x 1)
    1
    (* x (factorial (dec x)))
  )
)

(factorial 10)
`

var DEFAULT_PLAYGROUND_HEIGHT = 500
var DEFAULT_RESIZE_DIVIDER1_X_PERCENT = 25
var DEFAULT_RESIZE_DIVIDER2_X_PERCENT = 75

var moveParams = null
var playgroundHeight = null
var resizeDivider1XPercent = null
var resizeDivider2XPercent = null
var windowHeight = null
var windowWidth = null
var availablePanelsWidth = null

function calculateDimensions() {
  windowHeight = window.innerHeight
  windowWidth = window.innerWidth
  availablePanelsWidth = windowWidth - 26
}

function layout() {
  calculateDimensions()

  var wrapper = document.getElementById('wrapper')
  var playground = document.getElementById('playground')
  var sidebar = document.getElementById('sidebar')
  var mainPanel = document.getElementById('main-panel')
  var paramsTextarea = document.getElementById('params-textarea')
  var litsTextarea = document.getElementById('lits-textarea')
  var outputTextarea = document.getElementById('output-textarea')
  var paramsPanel = document.getElementById('params-panel')
  var litsPanel = document.getElementById('lits-panel')
  var outputPanel = document.getElementById('output-panel')
  var resizeDivider1 = document.getElementById('resize-divider-1')
  var resizeDivider2 = document.getElementById('resize-divider-2')

  var textAreaHeight = playgroundHeight - 77
  var topPanelsBottom = playgroundHeight + 8

  var paramsPanelWidth = (availablePanelsWidth * resizeDivider1XPercent) / 100
  var outputPanelWidth = (availablePanelsWidth * (100 - resizeDivider2XPercent)) / 100
  var litsPanelWidth = availablePanelsWidth - paramsPanelWidth - outputPanelWidth

  playground.style.height = playgroundHeight + 'px'
  resizeDivider1.style.height = playgroundHeight - 78 + 'px'
  resizeDivider2.style.height = playgroundHeight - 78 + 'px'
  resizeDivider1.style.marginTop = '20px'
  resizeDivider2.style.marginTop = '20px'
  paramsTextarea.style.height = textAreaHeight + 'px'
  litsTextarea.style.height = textAreaHeight + 'px'
  outputTextarea.style.height = textAreaHeight + 'px'

  paramsPanel.style.width = paramsPanelWidth + 'px'
  litsPanel.style.width = litsPanelWidth + 'px'
  outputPanel.style.width = outputPanelWidth + 'px'

  sidebar.style.bottom = topPanelsBottom + 'px'
  mainPanel.style.bottom = topPanelsBottom + 'px'

  wrapper.style.display = 'block'
}

function resetPlayground() {
  document.getElementById('params-textarea').value = ''
  document.getElementById('lits-textarea').value = ''
  document.getElementById('output-textarea').value = ''
  localStorage.removeItem('lits-textarea')
  localStorage.removeItem('params-textarea')
  localStorage.removeItem('playground-height')
  localStorage.removeItem('resize-divider-1-percent')
  localStorage.removeItem('resize-divider-2-percent')
  playgroundHeight = DEFAULT_PLAYGROUND_HEIGHT
  resizeDivider1XPercent = DEFAULT_RESIZE_DIVIDER1_X_PERCENT
  resizeDivider2XPercent = DEFAULT_RESIZE_DIVIDER2_X_PERCENT
  layout()
}

window.onload = function () {
  var storedPlaygroundHeight = localStorage.getItem('playground-height')
  var storedResizeDivider1XPercent = localStorage.getItem('resize-divider-1-percent')
  var storedResizeDivider2XPercent = localStorage.getItem('resize-divider-2-percent')

  playgroundHeight = storedPlaygroundHeight ? Number(storedPlaygroundHeight) : DEFAULT_PLAYGROUND_HEIGHT
  resizeDivider1XPercent = storedResizeDivider1XPercent
    ? Number(storedResizeDivider1XPercent)
    : DEFAULT_RESIZE_DIVIDER1_X_PERCENT
  resizeDivider2XPercent = storedResizeDivider2XPercent
    ? Number(storedResizeDivider2XPercent)
    : DEFAULT_RESIZE_DIVIDER2_X_PERCENT

  lits = new Lits.Lits({ debug: true })

  document.getElementById('resize-playground').onmousedown = event => {
    moveParams = {
      id: 'playground',
      startMoveY: event.clientY,
      heightBeforeMove: playgroundHeight,
    }
  }

  document.getElementById('resize-divider-1').onmousedown = event => {
    moveParams = {
      id: 'resize-divider-1',
      startMoveX: event.clientX,
      percentBeforeMove: resizeDivider1XPercent,
    }
  }

  document.getElementById('resize-divider-2').onmousedown = event => {
    moveParams = {
      id: 'resize-divider-2',
      startMoveX: event.clientX,
      percentBeforeMove: resizeDivider2XPercent,
    }
  }

  window.onresize = layout
  window.onmouseup = () => {
    document.body.classList.remove('no-select')
    moveParams = null
  }

  window.onmousemove = event => {
    if (moveParams === null) {
      return
    }

    document.body.classList.add('no-select')

    if (moveParams.id === 'playground') {
      playgroundHeight = moveParams.heightBeforeMove + moveParams.startMoveY - event.clientY
      if (playgroundHeight < 45) {
        playgroundHeight = 45
      }
      if (playgroundHeight > windowHeight - 89) {
        playgroundHeight = windowHeight - 89
      }
      localStorage.setItem('playground-height', playgroundHeight)
    } else if (moveParams.id === 'resize-divider-1') {
      resizeDivider1XPercent =
        moveParams.percentBeforeMove + ((event.clientX - moveParams.startMoveX) / availablePanelsWidth) * 100
      if (resizeDivider1XPercent < 10) {
        resizeDivider1XPercent = 10
      }
      if (resizeDivider1XPercent > resizeDivider2XPercent - 10) {
        resizeDivider1XPercent = resizeDivider2XPercent - 10
      }
      localStorage.setItem('resize-divider-1-percent', resizeDivider1XPercent)
    } else if (moveParams.id === 'resize-divider-2') {
      resizeDivider2XPercent =
        moveParams.percentBeforeMove + ((event.clientX - moveParams.startMoveX) / availablePanelsWidth) * 100
      if (resizeDivider2XPercent < resizeDivider1XPercent + 10) {
        resizeDivider2XPercent = resizeDivider1XPercent + 10
      }
      if (resizeDivider2XPercent > 90) {
        resizeDivider2XPercent = 90
      }
      localStorage.setItem('resize-divider-2-percent', resizeDivider2XPercent)
    }

    layout()
  }

  window.addEventListener('keydown', function (evt) {
    if (evt.key === 'F2') {
      evt.preventDefault()
      play()
    }
    if (evt.key === 'Escape') {
      evt.preventDefault()
      minimizeAll()
    }
  })
  document.getElementById('lits-textarea').addEventListener('keydown', keydownHandler)
  document
    .getElementById('lits-textarea')
    .addEventListener('input', e => localStorage.setItem('lits-textarea', e.target.value))
  document.getElementById('params-textarea').addEventListener('keydown', keydownHandler)
  document
    .getElementById('params-textarea')
    .addEventListener('input', e => localStorage.setItem('params-textarea', e.target.value))

  var id = location.hash.substring(1) || 'index'
  showPage(id, 'replace')

  var urlParams = new URLSearchParams(window.location.search)

  var program = urlParams.get('program')
  var litsTextArea = document.getElementById('lits-textarea')
  if (program) {
    litsTextArea.value = decodeURIComponent(program)
  } else {
    litsTextArea.value = localStorage.getItem('lits-textarea') || defaultProgram
  }

  var paramsTextArea = document.getElementById('params-textarea')
  paramsTextArea.value = localStorage.getItem('params-textarea') || ''

  layout()
  play()
}

function keydownHandler(e) {
  if (['Tab', 'Backspace', 'Enter', 'Delete'].includes(e.key)) {
    var start = this.selectionStart
    var end = this.selectionEnd

    let indexOfReturn = this.value.lastIndexOf('\n', start - 1)
    const rowLength = start - indexOfReturn - 1
    var onTabStop = rowLength % 2 === 0
    if (e.key == 'Tab') {
      e.preventDefault()
      if (!e.shiftKey) {
        this.value = this.value.substring(0, start) + (onTabStop ? '  ' : ' ') + this.value.substring(end)
        this.selectionStart = this.selectionEnd = start + (onTabStop ? 2 : 1)
      }
    }
    if (e.key == 'Backspace') {
      if (onTabStop && start === end && this.value.substr(start - 2, 2) === '  ') {
        e.preventDefault()
        this.value = this.value.substring(0, start - 2) + this.value.substring(end)
        this.selectionStart = this.selectionEnd = start - 2
      }
    }
    if (e.key == 'Enter') {
      e.preventDefault()
      const spaceCount = this.value.substring(indexOfReturn + 1, start).replace(/^( *).*/, '$1').length
      this.value = this.value.substring(0, start) + `\n${' '.repeat(spaceCount)}` + this.value.substring(end)
      this.selectionStart = this.selectionEnd = start + 1 + spaceCount
    }
    if (e.key == 'Delete') {
      if (onTabStop && start === end && this.value.substr(start, 2) === '  ') {
        e.preventDefault()
        this.value = this.value.substring(0, start) + this.value.substring(end + 2)
        this.selectionStart = this.selectionEnd = start
      }
    }
  }
}

window.addEventListener('popstate', () => {
  var id = location.hash.substring(1) || 'index'
  showPage(id, 'none')
})

function play() {
  var code = document.getElementById('lits-textarea').value
  var paramsString = document.getElementById('params-textarea').value
  var output = document.getElementById('output-textarea')
  output.value = ''
  var params
  try {
    params = paramsString.trim().length > 0 ? JSON.parse(paramsString) : {}
  } catch (e) {
    output.value = 'Error: Could not parse params'
    output.classList.add('error')
    return
  }
  var result
  var oldLog = console.log
  console.log = function () {
    var args = Array.from(arguments)
    oldLog.apply(console, args)
    var logRow = args.map(arg => stringifyValue(arg)).join(' ')
    var oldContent = output.value
    var newContent = oldContent ? oldContent + '\n' + logRow : logRow
    output.value = newContent
    output.scrollTop = output.scrollHeight
  }
  var oldWarn = console.warn
  console.warn = function () {
    var args = Array.from(arguments)
    oldWarn.apply(console, args)
    var logRow = args[0]
    var oldContent = output.value
    var newContent = oldContent ? oldContent + '\n' + logRow : logRow
    output.value = newContent
    output.scrollTop = output.scrollHeight
  }
  try {
    result = lits.run(code, params)
  } catch (error) {
    output.value = error
    output.classList.add('error')
    return
  } finally {
    console.log = oldLog
    console.warn = oldWarn
  }
  output.classList.remove('error')
  var content = stringifyValue(result)

  var oldContent = output.value
  var newContent = oldContent ? oldContent + '\n' + content : content
  output.value = newContent
  output.scrollTop = output.scrollHeight

  output.value = newContent
}
function showPage(id, historyEvent) {
  inactivateAll()

  const page = document.getElementById(id)
  const link = document.getElementById(id + '_link')

  if (page) {
    page.classList.add('active-content')
    if (link) {
      link.classList.add('active-sidebar-entry')
    }
  } else {
    showPage('index', 'replace')
    return
  }

  if (historyEvent === 'none') {
    return
  } else if (historyEvent === 'replace') {
    history.replaceState(null, '', '#' + id)
  } else {
    history.pushState(null, '', '#' + id)
  }
}

function inactivateAll() {
  var els = document.getElementsByClassName('active-content')
  while (els[0]) {
    els[0].classList.remove('active-content')
  }
  els = document.getElementsByClassName('active-sidebar-entry')
  while (els[0]) {
    els[0].classList.remove('active-sidebar-entry')
  }
}

function stringifyValue(value) {
  if (Lits.isLitsFunction(value)) {
    if (value.builtin) {
      return `<builtin function ${value.builtin}>`
    } else {
      return `<function ${value.name || 'λ'}>`
    }
  }
  if (value === null) {
    return `null`
  }
  if (typeof value === 'object' && value instanceof RegExp) {
    return `${value}`
  }
  if (typeof value === 'object' && value instanceof Error) {
    return value.toString()
  }
  return JSON.stringify(value)
}

function addToPlayground(example) {
  example = example.replace(/___single_quote___/g, "'").replace(/___double_quote___/g, '"')
  var textarea = document.getElementById('lits-textarea')

  if (textarea.value) {
    textarea.value = textarea.value + `\n\n${example}`
  } else {
    textarea.value = example
  }

  localStorage.setItem('lits-textarea', textarea.value)
}

function setPlayground(exampleId) {
  const example = examples.find(ex => ex.id === exampleId)
  if (!example) {
    throw Error(`Could not find example '${exampleId}'`)
  }

  if (example.params) {
    const value = JSON.stringify(example.params, (_k, v) => (v === undefined ? null : v), 2)
    document.getElementById('params-textarea').value = value
    localStorage.setItem('params-textarea', value)
  }

  if (example.code) {
    document.getElementById('lits-textarea').value = example.code
    localStorage.setItem('lits-textarea', example.code)
  }

  play()
}
