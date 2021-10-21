;(function () {
  lispish = new Lispish.Lispish()

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
  document.getElementById('lisp-textarea').addEventListener('keydown', keydownHandler)
  document
    .getElementById('lisp-textarea')
    .addEventListener('input', e => localStorage.setItem('lisp-textarea', e.target.value))
  document.getElementById('context-textarea').addEventListener('keydown', keydownHandler)
  document
    .getElementById('context-textarea')
    .addEventListener('input', e => localStorage.setItem('context-textarea', e.target.value))

  var id = location.hash.substring(1) || 'index'
  showPage(id, 'replace')

  document.getElementById('lisp-textarea').value = localStorage.getItem('lisp-textarea') || ''
  document.getElementById('context-textarea').value = localStorage.getItem('context-textarea') || ''
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
  var code = document.getElementById('lisp-textarea').value
  var contextString = document.getElementById('context-textarea').value
  var output = document.getElementById('output-textarea')
  output.value = ''
  var context
  try {
    context = contextString.trim().length > 0 ? JSON.parse(contextString) : {}
  } catch (e) {
    output.value = 'Error: Could not parse context'
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
    result = lispish.run(code, { vars: context })
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
  if (Lispish.isLispishFunction(value)) {
    if (value.builtin) {
      return `<builtin function ${value.builtin}>`
    } else {
      return `<function ${value.name || 'λ'}>`
    }
  }
  if (value === null) {
    return `nil`
  }
  if (typeof value === 'object' && value instanceof RegExp) {
    return `${value}`
  }
  if (typeof value === 'object' && value instanceof Error) {
    return value.toString()
  }
  return JSON.stringify(value, (k, v) => (v === undefined ? 'b234ca78-ccc4-5749-9384-1d3415d29423' : v)).replace(
    /"b234ca78-ccc4-5749-9384-1d3415d29423"/g,
    'undefined',
  )
}

function resetPlayground() {
  document.getElementById('context-textarea').value = ''
  document.getElementById('lisp-textarea').value = ''
  document.getElementById('output-textarea').value = ''
  localStorage.setItem('lisp-textarea', '')
  localStorage.setItem('context-textarea', '')
}

function addToPlayground(example) {
  example = example.replace(/___single_quote___/g, "'").replace(/___double_quote___/g, '"')
  var textarea = document.getElementById('lisp-textarea')

  if (textarea.value) {
    textarea.value = textarea.value + `\n\n${example}`
  } else {
    textarea.value = example
  }

  localStorage.setItem('lisp-textarea', textarea.value)
}

function setPlayground(exampleId) {
  const example = examples.find(ex => ex.id === exampleId)
  if (!example) {
    throw Error(`Could not find example '${exampleId}'`)
  }

  resetPlayground()

  if (example.context) {
    const value = JSON.stringify(example.context, null, 2)
    document.getElementById('context-textarea').value = value
    localStorage.setItem('context-textarea', value)
  }

  if (example.code) {
    document.getElementById('lisp-textarea').value = example.code
    localStorage.setItem('lisp-textarea', example.code)
  }
}

function maximizeContext() {
  minimizeAll()
  document.body.classList.add('maximized-context')
}

function maximizeLisp() {
  minimizeAll()
  document.body.classList.add('maximized-lisp')
}

function maximizeOutput() {
  minimizeAll()
  document.body.classList.add('maximized-output')
}

function minimizeAll() {
  document.body.classList.remove('maximized-context')
  document.body.classList.remove('maximized-lisp')
  document.body.classList.remove('maximized-output')
}

function toggleMaximized(textArea) {
  if (textArea.id === 'lisp-textarea') {
    if (document.body.classList.contains('maximized-lisp')) {
      minimizeAll()
    } else {
      maximizeLisp()
    }
  } else if (textArea.id === 'context-textarea') {
    if (document.body.classList.contains('maximized-context')) {
      minimizeAll()
    } else {
      maximizeContext()
    }
  }
}
