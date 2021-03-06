/* eslint-disable no-console */
/* eslint-disable no-unused-vars */
/* eslint-disable no-undef */
;(function () {
  lits = new Lits.Lits({ debug: true })

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

  document.getElementById('lits-textarea').value = localStorage.getItem('lits-textarea') || ''
  document.getElementById('params-textarea').value = localStorage.getItem('params-textarea') || ''
})()

function keydownHandler(e) {
  if (e.key === 'F3') {
    toggleMaximized(this)
    e.preventDefault()
    return
  }
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
      return `<function ${value.name || '??'}>`
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

function resetPlayground() {
  document.getElementById('params-textarea').value = ''
  document.getElementById('lits-textarea').value = ''
  document.getElementById('output-textarea').value = ''
  localStorage.setItem('lits-textarea', '')
  localStorage.setItem('params-textarea', '')
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

  resetPlayground()

  if (example.params) {
    const value = JSON.stringify(example.params, (_k, v) => (v === undefined ? null : v), 2)
    document.getElementById('params-textarea').value = value
    localStorage.setItem('params-textarea', value)
  }

  if (example.code) {
    document.getElementById('lits-textarea').value = example.code
    localStorage.setItem('lits-textarea', example.code)
  }
}

function maximizeContext() {
  minimizeAll()
  document.body.classList.add('maximized-params')
}

function maximizeLisp() {
  minimizeAll()
  document.body.classList.add('maximized-lits')
}

function maximizeOutput() {
  minimizeAll()
  document.body.classList.add('maximized-output')
}

function minimizeAll() {
  document.body.classList.remove('maximized-params')
  document.body.classList.remove('maximized-lits')
  document.body.classList.remove('maximized-output')
}

function toggleMaximized(textArea) {
  if (textArea.id === 'lits-textarea') {
    if (document.body.classList.contains('maximized-lits')) {
      minimizeAll()
    } else {
      maximizeLisp()
    }
  } else if (textArea.id === 'params-textarea') {
    if (document.body.classList.contains('maximized-params')) {
      minimizeAll()
    } else {
      maximizeContext()
    }
  }
}
