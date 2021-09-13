const { functionReference, categories } = require('../../cli/reference')
const lispish = require('../../dist/lispish')
const path = require('path')
const fs = require('fs')

const DOC_DIR = path.resolve(__dirname, '../../docs')

setupDocDir()
copyScripts()
copyStyles()
writeIndexPage()

function writeIndexPage() {
  const page = `<!DOCTYPE html>
<html lang="en">
${getHeader()}
<body>
  ${getTopBar({ back: false })}
  <div id="page" class="row">
    ${getSideBar()}
    <main id="main-panel">
      ${getIndexContent()}
      ${Object.values(functionReference)
        .map(obj => getDocumentationContent(obj))
        .join('\n')}
    </main>
  </div>
  ${getPlayground()}
  <script src="https://cdn.jsdelivr.net/npm/lodash@4.17.21/lodash.min.js" ></script>
  <script src="lispish.iife.js"></script>
  <script src='scripts.js'></script>
</body>
</html>
`
  fs.writeFileSync(path.join(DOC_DIR, `index.html`), page, { encoding: 'utf-8' })
}

function getTopBar() {
  return `
<header id="top-bar">
  <div class="row">
    <div class="column">
      <a id="home-link" onclick="showPage('index')">Home</a>
    </div>
    <div class="column header">Lispish</div>
    <div class="column">
    </div>
  </div>
</header>`
}

function getHeader() {
  return `
<head>
  <title>Lispish</title>
  <meta name="description" content="A reference and a playground for Lispish - a Typescript Lisp implementation">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="styles.css">
</head>
`
}

function getPlayground() {
  return `
<div id="playground">
  <div class="header"><span class="icon-button" onclick="play()">▶ Playground</span></div>
  <div class="row">
    <div class="column" id="context">
      <div class="textarea-header"><label for="context-textarea">Context (JSON)</label></div>
      <textarea spellcheck=false rows="12" id="context-textarea">{ "x": 12 }</textarea>
    </div>
    <div class="column" id="lisp">
      <div class="textarea-header"><label for="lisp-textarea">Lisp</label></div>
      <textarea spellcheck=false rows="12" id="lisp-textarea">(setq y 5)\n\n(write "y" y)\n\n(write (* x y))</textarea>
    </div>
    <div class="column" id="output">
      <div class="row">
        <div class="column textarea-header"><label for="output-textarea">Result</label></div>
        <div class="column small right"><span id="clear-output" class="icon-button" onclick="clearOutput()">✖</span></div>
      </div>
      <textarea id="output-textarea" readonly spellcheck=false rows="12" id="context"></textarea>
    </div>
    <div class="column" id="log">
      <div class="row">
        <div class="column textarea-header"><label for="log-textarea">Console log</label></div>
        <div class="column small right"><span id="clear-log" class="icon-button" onclick="clearLog()">✖</span></div>
      </div>
      <textarea id="log-textarea" readonly spellcheck=false rows="12" id="context"></textarea>
    </div>
  </div>
</div>
`
}

function getIndexContent() {
  return `
<div id="index" class="content">
  <h1>Welcome to Lispish!</h1>
  <br />
  <p>Lispish is a Lisp dialect made to work well in a browser or Node environment.</p>
  <p>Quite a lot in Lispish is not what you're used to if you've done some Lisp before.</p>
  <ul>
    <li><pre>t</pre> and <pre>nil</pre> are gone. Instead there are four new symbols: <pre>true</pre>, <pre>false</pre>, <pre>null</pre> and <pre>undefined</pre>.</li>
    <li>Only one sequence type exists: <pre>list</pre>. And its undelaying data structure is a javascript array.</li>
    <li>No quotes! <pre>'(1 2 3)</pre> is no more... Use <pre>(list 1 2 3)</pre> instead.</li>
    <li>No macros.</li>
    <li>No keyword symbols e.g. <pre>:foo</pre>.</li>
    <li>No tail call optimization (yet).</li>
    <li>No dotted pairs.</li>
    <li>100% test coverage</li>
  </ul>
  <p>Have a look at the list of functions to the left. These are what is available in terms of special- and normal expressions.</p>
  <p>For more instruction on how to install and use Lispish as a cli or a typescript lib, checkout <a href="https://github.com/mojir/lispish">https://github.com/mojir/lispish</a></p>
  <p/>
  <p>Happy coding!</p>
</div>
`
}

function getDocumentationContent(docObj) {
  const { name, longDescription, returns, linkName, specialExpression, examples, sideEffects, arguments: args } = docObj
  const formattedDescription = formatDescription(longDescription)
  return `
<div id="${linkName}" class="content function">
  <div class="function-header">${name}</div>
  ${specialExpression ? '<h3>Special Expression</h3>' : ''}
  <p>${formattedDescription}</p>
  <label>Syntax</label>
  <pre>${getSyntax(name, args, returns)}</pre>
  <label>Arguments</label>
  <pre>${args.length === 0 ? 'No arguments' : args.map(arg => `${arg.name}: ${arg.type}`).join('<br />')}</pre>
  <label>Side effects</label>
  <pre>${sideEffects.length === 0 ? 'No side effects' : sideEffects.map(effect => effect).join('<br />')}</pre>
  <label>Examples</label>
  ${examples
    .map(example => {
      var oldLog = console.log
      console.log = function () {}
      var result
      try {
        result = lispish.lispish(example)
        return `<pre>${example} => ${stringifyValue(result)}</pre>`
      } catch (error) {
        return `<pre>${example} => Error!</pre>`
      } finally {
        console.log = oldLog
      }
    })
    .join('\n')}
</div>
`
}

function getSideBar() {
  const categoryCollections = Object.values(functionReference).reduce((result, obj) => {
    result[obj.category] = result[obj.category] || []
    result[obj.category].push(obj)
    return result
  }, {})

  return `
<nav id="sidebar">
  ${categories
    .map(categoryKey => {
      return `
        <label>${categoryKey}</label>
        <ul>
          ${
            categoryCollections[categoryKey]
              ? categoryCollections[categoryKey]
                  .map(obj => {
                    const linkName = obj.linkName
                    const name = escape(obj.name)
                    return `<li id="${linkName}_link" onclick="showPage('${linkName}')">${name}</li>`
                  })
                  .join('\n')
              : ''
          }
        </ul>`
    })
    .join('\n')}
</nav>
`
}

function setupDocDir() {
  fs.rmdirSync(DOC_DIR, { recursive: true, force: true })
  fs.mkdirSync(DOC_DIR)
}

function copyScripts() {
  fs.copyFileSync(path.join(__dirname, '../../dist/lispish.iife.js'), path.join(DOC_DIR, 'lispish.iife.js'))
  fs.copyFileSync(path.join(__dirname, `scripts.js`), path.join(DOC_DIR, `scripts.js`))
}

function copyStyles() {
  fs.copyFileSync(path.join(__dirname, `styles.css`), path.join(DOC_DIR, `styles.css`))
}

function escape(str) {
  str = str.replace(/>/g, '&gt;')
  str = str.replace(/</g, '&lt;')
  return str
}

function stringifyValue(value) {
  return JSON.stringify(value, (k, v) => (v === undefined ? 'b234ca78-ccc4-5749-9384-1d3415d29423' : v)).replace(
    /"b234ca78-ccc4-5749-9384-1d3415d29423"/g,
    'undefined',
  )
}

function formatDescription(value) {
  const logThis = value.indexOf('***') >= 0
  value = value.replace(/`(.*?)`/g, '<span class="pre">$1</span>')
  value = value.replace(/\*\*\*(.*?)\*\*\*/g, '<strong><em>$1</em></strong>')
  value = value.replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>')
  value = value.replace(/\*(.*?)\*/g, '<em>$1</em>')
  if (logThis) {
    console.log(value)
  }
  return value
}

function getSyntax(name, args, returns) {
  return `${name}${
    args.length ? ' ' + args.map(arg => `${arg.name}${arg.description ? `(${arg.description})` : ''}`).join(' ') : ''
  } => ${returns.type}`
}
