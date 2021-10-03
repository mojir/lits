const { version } = require('../../package.json')
const { functionReference, categories } = require('../../cli/reference')
const examples = require('./examples')
const Lispish = require('../../dist/index')
const path = require('path')
const fs = require('fs')

const DOC_DIR = path.resolve(__dirname, '../../docs')
const lispish = new Lispish.Lispish()

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
  <script src="lispish.iife.js"></script>
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
    <div class="column header">Lispish</div>
    <div class="column right version"><span>v${version}</span></div>
  </div>
</header>`
}

function getHtmlHeader() {
  return `
<head>
  <title>Lispish</title>
  <link rel='shortcut icon' type='image/x-icon' href='favicon.ico' />
  <meta name="description" content="A reference and a playground for Lispish - a Typescript Lisp implementation">
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
      <span class="button" onclick="play()">Run [Ctrl-Enter]</span>
      <span class="button" onclick="resetPlayground()">Reset</span>
      </center>
    </div>
    <div class="column right">
      <span id="maximize-playground" class="icon-button" onclick="maximizePlayground()">▲</span>
      <span id="minimize-playground" class="icon-button" onclick="minimizePlayground()">▼</span>
    </div>
  </div>
  <div class="row">
    <div class="column" id="context">
      <div class="textarea-header"><label for="context-textarea">Context (JSON)</label></div>
      <textarea id="context-textarea" class="fancy-scroll" spellcheck="false"></textarea>
    </div>
    <div class="column wider" id="lisp">
      <div class="textarea-header"><label for="lisp-textarea">Lisp</label></div>
      <textarea id="lisp-textarea" class="fancy-scroll" spellcheck="false"></textarea>
    </div>
    <div class="column wide" id="output">
      <div class="textarea-header"><label for="output-textarea">Result</label></div>
      <textarea id="output-textarea" class="fancy-scroll" readonly spellcheck="false" ></textarea>
      <div class="textarea-header"><label for="log-textarea">Console</label></div>
      <textarea id="log-textarea" class="fancy-scroll" readonly spellcheck="false" ></textarea>
    </div>
  </div>
</div>
`
}

function getIndexPage() {
  return `
<div id="index" class="content">
  <h1>Welcome to Lispish!</h1>
  <br />
    <div class="text">
    <p>Lispish is a Lisp dialect made to work well in a browser or Node environment.</p>
    <p>Quite a lot in Lispish is not what you're used to if you've done some Lisp before.</p>
    <ul>
      <li><pre>t</pre> and <pre>nil</pre> are gone. Instead there are four new symbols: <pre>true</pre>, <pre>false</pre>, <pre>null</pre> and <pre>undefined</pre>.</li>
      <li>Only one sequence type exists: <pre>list</pre>. And its undelaying data structure is a javascript array.</li>
      <li>Quotes behave differently! <pre>'</pre> is simply a short hand for <pre>(list ...)</pre>.</li>
      <li>No macros.</li>
      <li>No keyword symbols e.g. <pre>:foo</pre>.</li>
      <li>No tail call optimization (yet).</li>
      <li>No dotted pairs.</li>
      <li>100% test coverage</li>
    </ul>
    <p>You can see some examples and find documentation of all built-in function to the left.</p>
    <p>For more instruction on how to install and use Lispish as a cli or a typescript lib, checkout <a href="https://github.com/mojir/lispish">https://github.com/mojir/lispish</a></p>
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
  const { name, longDescription, returns, linkName, specialExpression, examples, sideEffects, arguments: args } = docObj
  const formattedDescription = formatDescription(longDescription)
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

  ${
    sideEffects.length === 0
      ? '<label>No side effects</label>'
      : `<label>Side effects</label><div class="indent">${sideEffects
          .map(effect => `<pre>${effect}</pre>`)
          .join('\n')}</div>`
  }
  <label>Examples</label>
  <div class="indent">
    ${examples
      .map(example => {
        var oldLog = console.log
        console.log = function () {}
        var oldError = console.error
        console.error = function () {}
        var result
        try {
          result = lispish.run(example)

          return `<pre><span class="example" onclick="addToPlayground('${escapeExample(
            example,
          )}')"><span class="icon-button">▶</span> ${example} <span class="gray">=> ${stringifyValue(
            result,
          )}</span></span></pre>`
        } catch (error) {
          return `<pre class="example" onclick="addToPlayground('${escapeExample(
            example,
          )}')"><span class="icon-button">▶</span> ${example} <span class="gray">=></span> <span class="error">Error!</span></pre>`
        } finally {
          console.log = oldLog
          console.error = oldError
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
  ${categories
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
  fs.copyFileSync(path.join(__dirname, '../../dist/lispish.iife.js'), path.join(DOC_DIR, 'lispish.iife.js'))
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
  if (Lispish.isLispishFunction(value)) {
    if (value.builtin) {
      return `&lt;builtin function ${value.builtin}&gt;`
    } else {
      return `&lt;function ${value.name ?? 'λ'}&gt;`
    }
  }
  if (typeof value === 'object' && value instanceof Error) {
    return value.toString()
  }
  if (typeof value === 'object' && value instanceof RegExp) {
    return `${value}`
  }
  return JSON.stringify(value, (k, v) => (v === undefined ? 'b234ca78-ccc4-5749-9384-1d3415d29423' : v)).replace(
    /"b234ca78-ccc4-5749-9384-1d3415d29423"/g,
    'undefined',
  )
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
