;(function () {
  lispish = new Lispish.Lispish()

  window.addEventListener('keyup', function (evt) {
    if (evt.key === 'Enter' && evt.ctrlKey === true) {
      play()
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
  var logTextarea = document.getElementById('log-textarea')
  logTextarea.value = ''
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
    var oldContent = logTextarea.value
    var newContent = oldContent ? oldContent + '\n' + logRow : logRow
    logTextarea.value = newContent
    logTextarea.scrollTop = logTextarea.scrollHeight
  }
  var oldError = console.error
  console.error = function () {
    var args = Array.from(arguments)
    oldError.apply(console, args)
    var logRow = args.map(arg => stringifyValue(arg)).join(' ')
    var oldContent = logTextarea.value
    var newContent = oldContent ? oldContent + '\n' + logRow : logRow
    logTextarea.value = newContent
    logTextarea.scrollTop = logTextarea.scrollHeight
  }
  try {
    result = lispish.run(code, { vars: context })
  } catch (error) {
    output.value = error
    output.classList.add('error')
    return
  } finally {
    console.log = oldLog
    console.error = oldError
  }
  output.classList.remove('error')
  var content = stringifyValue(result)
  output.value = content
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
      return `<BUILTIN FUNCTION ${value.builtin}>`
    } else {
      return `<FUNCTION ${value.name || 'λ'}>`
    }
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
  document.getElementById('log-textarea').value = ''
  localStorage.setItem('lisp-textarea', '')
  localStorage.setItem('context-textarea', '')
}

function runPlayground(example) {
  resetPlayground()
  example = example.replace(/___single_quote___/g, "'").replace(/___double_quote___/g, '"')
  var textarea = document.getElementById('lisp-textarea')
  textarea.value = example
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

function maximizePlayground() {
  document.body.classList.add('maximized-playground')
}
function minimizePlayground() {
  document.body.classList.remove('maximized-playground')
}
