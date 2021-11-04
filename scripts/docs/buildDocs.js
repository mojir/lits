/* eslint-disable no-console */
const { version } = require('../../package.json')
const { functionReference, categorizedFunctions } = require('../../cli/reference')
const examples = require('./examples')
const Lits = require('../../dist/index')
const path = require('path')
const fs = require('fs')

const DOC_DIR = path.resolve(__dirname, '../../docs')
const lits = new Lits.Lits({ debug: true })

setupDocDir()
copyScripts()
copyStyles()
copyFavicon()
writeIndexPage()

function writeIndexPage() {
  const page = `<!DOCTYPE html>
<html lang="en">
${getHtmlHeader()}
<body>
  ${getTopBar({ back: false })}
  <main id="main-panel" class="fancy-scroll">
    ${getIndexPage()}
    ${getExamplePage()}
    ${Object.values(functionReference)
      .map(obj => getDocumentationContent(obj))
      .join('\n')}
  </main>
  ${getSideBar()}
  ${getPlayground()}
  <script src="lits.iife.js"></script>
  <script src='examples.js'></script>
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
    <div class="column"></div>
    <div class="column header">Lits</div>
    <div class="column right version"><span>v${version}</span></div>
  </div>
</header>`
}

function getHtmlHeader() {
  return `
<head>
  <title>Lits</title>
  <link rel='shortcut icon' type='image/x-icon' href='favicon.ico' />
  <meta name="description" content="A reference and a playground for Lits - a Typescript Lisp implementation">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="styles.css">
</head>
`
}

function getPlayground() {
  return `
<div id="playground">
  <div class="header row">
    <div class="column">Playground</span></div>
    <div class="column">
      <center>
      <span class="button" onclick="play()">Run [F2]</span>
      <span class="button" onclick="resetPlayground()">Reset</span>
      </center>
    </div>
    <div class="column right">
    </div>
  </div>
  <div class="row">
    <div class="column" id="params">
      <div class="row">
        <div class="column textarea-header"><label for="params-textarea">Params (JSON)</label></div>
        <div class="column right">
          <span id="maximize-params" class="icon-button" onclick="maximizeContext()">▲</span>
          <span id="minimize-params" class="icon-button" onclick="minimizeAll()">▼</span>
        </div>
      </div>
      <textarea id="params-textarea" class="fancy-scroll" spellcheck="false"></textarea>
    </div>

    <div class="column wider" id="lits">
      <div class="row">
        <div class="column textarea-header"><label for="lits-textarea">Lisp</label></div>
        <div class="column right">
          <span id="maximize-lits" class="icon-button" onclick="maximizeLisp()">▲</span>
          <span id="minimize-lits" class="icon-button" onclick="minimizeAll()">▼</span>
        </div>
      </div>
      <textarea id="lits-textarea" class="fancy-scroll" spellcheck="false"></textarea>
    </div>

    <div class="column wide" id="output">
      <div class="row">
        <div class="column textarea-header"><label for="output-textarea">Result</label></div>
        <div class="column right">
          <span id="maximize-output" class="icon-button" onclick="maximizeOutput()">▲</span>
          <span id="minimize-output" class="icon-button" onclick="minimizeAll()">▼</span>
        </div>
      </div>
      <textarea id="output-textarea" class="fancy-scroll" readonly spellcheck="false" ></textarea>
    </div>
  </div>
</div>
`
}

function getIndexPage() {
  return `
<div id="index" class="content">
  <h1>Welcome to Lits!</h1>
  <br />
    <div class="text">
    <p>Lits is a Lisp dialect made to work well in a browser or Node environment. It's heavily inspired by Clojure, most of the core functions from Clojure have been ported.</p>
    <p>Some outstanding features / shortcommings worth mentioning.</p>
    <ul>
      <li>All datatypes in Lits are immutable.</li>
      <li>All functions are <a href="https://www.sitepoint.com/functional-programming-pure-functions/">pure</a>, unless the built-in function name ends with a !. See <pre>write!</pre> or <pre>rand!</pre> for example.</li>
      <li>All datatypes in Lits mapps directly to Javascript's types.</li>
      <li>No lazy evaluation.</li>
      <li>No quotes.</li>
      <li>No macros.</li>
      <li>No keyword symbols. <pre>:foo</pre> is just a shourthand for <pre>'foo'</pre>.</li>
      <li>Dynamic scoping, no lexical scoping</li>
      <li>Strings look like <pre>'A string'</pre>, not <pre>"A string"</pre>. This desition was made to make it more convenient to embed lits code in json-files.</li>
      <li>100% test coverage</li>
    </ul>
    <p>You can see some examples and find documentation of all built-in function to the left.</p>
    <p>For more instruction on how to install and use Lits as a cli or a typescript lib, checkout <a href="https://github.com/mojir/lits">https://github.com/mojir/lits</a></p>
    <p/>
    <p>Happy coding!</p>
  </div>
</div>
`
}

function getExamplePage() {
  return `
<div id="example-page" class="content">
  <h1>Examples</h1>
  <br />
  <ul>
  ${examples
    .map(example => {
      return `
      <li>
        <div class="row example-item">
          <div class="column wide">
            <div class="example-name">${example.name}</div>
            <div class="example-description">${example.description}</div>
          </div>
          <div class="column right">
            <span class="button" onclick="setPlayground('${example.id}')">Show in playground</span>
          </div>
        </div>
      </li>
    `
    })
    .join('\n')}
  </ul>
</div>
`
}

function getDocumentationContent(docObj) {
  const { name, description, returns, linkName, specialExpression, examples, arguments: args } = docObj
  const formattedDescription = formatDescription(description)
  return `
<div id="${linkName}" class="content function">
  <div class="function-header">${name}</div>
  ${specialExpression ? '<h3>Special Expression</h3>' : ''}
  <p>${formattedDescription}</p>
  <label>Syntax</label>
  <div class="indent">
    <pre>${getSyntax(name, args, returns)}</pre>
  </div>

  ${
    args.length === 0
      ? '<label>No arguments</label>'
      : `<label>Arguments</label><div class="indent">${args
          .map(arg => `<pre>${arg.name}: ${arg.type}</pre>`)
          .join('\n')}</div>`
  }

  <label>Examples</label>
  <div class="indent">
    ${examples
      .map(example => {
        var oldLog = console.log
        console.log = function () {}
        var oldWarn = console.warn
        console.warn = function () {}
        var result
        var escapedExample = escapeExample(example)
        try {
          result = lits.run(example)
          var stringifiedResult = stringifyValue(result)

          return `<pre><span class="example" onclick="addToPlayground('${escapedExample}')"> <span class="icon-button">▶</span> ${example} <span class="gray">=> ${stringifiedResult} </span> </span></pre>`
        } finally {
          console.log = oldLog
          console.warn = oldWarn
        }
      })
      .join('\n')}
  </div>
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
<nav id="sidebar" class="fancy-scroll">
  <label class="link" onclick="showPage('index')">Home</label>
  <br />
  <label class="link" onclick="showPage('example-page')">Examples</label>
  <br />
  ${categorizedFunctions
    .map(categoryKey => {
      return `
        <label>${categoryKey}</label>
        <ul>
          ${
            categoryCollections[categoryKey]
              ? categoryCollections[categoryKey]
                  .sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0))
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
  fs.copyFileSync(path.join(__dirname, '../../dist/lits.iife.js'), path.join(DOC_DIR, 'lits.iife.js'))
  fs.copyFileSync(path.join(__dirname, `scripts.js`), path.join(DOC_DIR, `scripts.js`))
  const examplesContent = fs.readFileSync(path.join(__dirname, `examples.js`), { encoding: 'utf-8' })
  fs.writeFileSync(path.join(DOC_DIR, `examples.js`), examplesContent.replace('module.exports =', 'var examples ='))
}

function copyStyles() {
  fs.copyFileSync(path.join(__dirname, `styles.css`), path.join(DOC_DIR, `styles.css`))
}

function copyFavicon() {
  fs.copyFileSync(path.join(__dirname, `favicon.ico`), path.join(DOC_DIR, `favicon.ico`))
}

function stringifyValue(value) {
  if (Lits.isLitsFunction(value)) {
    if (value.builtin) {
      return `&lt;builtin function ${value.builtin}&gt;`
    } else {
      return `&lt;function ${value.name || 'λ'}&gt;`
    }
  }
  if (value === null) {
    return `null`
  }
  if (typeof value === 'object' && value instanceof Error) {
    return value.toString()
  }
  if (typeof value === 'object' && value instanceof RegExp) {
    return `${value}`
  }
  return JSON.stringify(value)
}

function escape(str) {
  str = str.replace(/>/g, '&gt;')
  str = str.replace(/</g, '&lt;')
  return str
}

function formatDescription(value) {
  value = value.replace(/`(.*?)`/g, '<span class="pre">$1</span>')
  value = value.replace(/\*\*\*(.*?)\*\*\*/g, '<strong><em>$1</em></strong>')
  value = value.replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>')
  value = value.replace(/\*(.*?)\*/g, '<em>$1</em>')
  return value
}

function getSyntax(name, args, returns) {
  return `${name}${
    args.length ? ' ' + args.map(arg => `${arg.name}${arg.description ? `(${arg.description})` : ''}`).join(' ') : ''
  } => ${returns.type}`
}

function escapeExample(example) {
  return example.replace(/'/g, '___single_quote___').replace(/"/g, '___double_quote___')
}
