;(function () {
  window.addEventListener('keyup', function (evt) {
    if (evt.key === 'Enter' && evt.ctrlKey === true) {
      play()
    }
  })
  var id = location.hash.substring(1) || 'index'
  showPage(id, 'replace')
})()

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
    var logRow = args.map(arg => stringifyValue(arg)).join(' ')
    var oldContent = logTextarea.value
    var newContent = oldContent ? oldContent + '\n' + logRow : logRow
    logTextarea.value = newContent
    logTextarea.scrollTop = logTextarea.scrollHeight
  }
  try {
    result = lispish.lispish(code, context)
  } catch (error) {
    output.innerHTML = error
    output.classList.add('error')
    return
  } finally {
    console.log = oldLog
  }
  output.classList.remove('error')
  var content = stringifyValue(result)
  output.innerHTML = content
  if (content) {
    document.getElementById('clear-output').classList.add('active')
  }
  if (logTextarea.value) {
    document.getElementById('clear-log').classList.add('active')
  } else {
    document.getElementById('clear-log').classList.remove('active')
  }
}
function showPage(id, historyEvent) {
  var els = document.getElementsByClassName('active-content')
  while (els[0]) {
    els[0].classList.remove('active-content')
  }
  els = document.getElementsByClassName('active-sidebar-entry')
  while (els[0]) {
    els[0].classList.remove('active-sidebar-entry')
  }

  if (id === 'index') {
    document.getElementById('home-link').classList.remove('active')
    document.getElementById('index').classList.add('active-content')
  } else {
    var docPage = document.getElementById(id)
    if (docPage) {
      docPage.classList.add('active-content')
      document.getElementById(id + '_link').classList.add('active-sidebar-entry')
    } else {
      showPage('index', 'replace')
      return
    }
    document.getElementById('home-link').classList.add('active')
  }
  if (historyEvent === 'none') {
    return
  } else if (historyEvent === 'replace') {
    history.replaceState(null, '', '#' + id)
  } else {
    history.pushState(null, '', '#' + id)
  }
}

function stringifyValue(value) {
  return JSON.stringify(value, (k, v) => (v === undefined ? 'b234ca78-ccc4-5749-9384-1d3415d29423' : v)).replace(
    /"b234ca78-ccc4-5749-9384-1d3415d29423"/g,
    'undefined',
  )
}

function clearOutput() {
  document.getElementById('output-textarea').innerHTML = ''
  document.getElementById('clear-output').classList.remove('active')
  document.getElementById('log-textarea').innerHTML = ''
  document.getElementById('clear-log').classList.remove('active')
}

function runPlayground(example) {
  example = example.replace(/___single_quote___/g, "'").replace(/___double_quote___/g, '"')
  var textarea = document.getElementById('lisp-textarea')
  textarea.value = example
  play()
}
