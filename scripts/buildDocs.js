const reference = require('../cli/reference')
const path = require('path')
const fs = require('fs')

const DOC_DIR = path.resolve(__dirname, '../docs')

const getTopBar = ({ back }) => `<div class="top-bar">
  <center><pre><span class="main-header"><a id="lispish-header" onclick="setActive('index')">Lispish</a></span></pre></center>
</div>`

const getHeader = () => `<head>
  <link rel="stylesheet" href="styles.css">
</head>`

const getPlayground = () => `  <div class="playground">
    <div class="header">
      Playground <span class="play" onclick="play()">&#9654;</span>
    </div>
    <div class="row">
      <div class="column-half">
        <h4>Lisp</h4>
        <textarea spellcheck=false rows="20" id="code">(* x x)</textarea>
      </div>
      <div class="column-half">
        <h4>Context (JSON)</h4>
        <textarea spellcheck=false rows="20" id="context">{ "x": 12 }</textarea>
      </div>
    </div>
    <div id="output" />
  </div>`

const getScriptTags = () => `  <script src="https://cdn.jsdelivr.net/npm/lodash@4.17.21/lodash.min.js" ></script>
  <script src="lispish.iife.js"></script>
  <script>
    var activeId='index';
    function play() {
      var code = document.getElementById("code").value
      var contextString = document.getElementById("context").value
      var output = document.getElementById("output")
      var context
      try {
        context = JSON.parse(contextString)
      } catch (e) {
        output.innerHTML = "ERROR: Could not parse context"
        output.classList.add('error')
        return
      }
      var result
      try {
        result = lispish.lispish(code, context)
      } catch (error) {
        output.innerHTML = "LISPISH ERROR" + error
        output.classList.add('error')
        return
      }
      output.classList.remove('error')
      output.innerHTML = JSON.parse(result)
    }
    function setActive(id) {
      document.getElementById(activeId).classList.remove('active')
      document.getElementById(id).classList.add('active')
      if (activeId !== 'index') {
        document.getElementById(activeId + '_link').classList.remove('active-sidebar-entry')
      }
      if (id !== 'index') {
        console.log(id + '_link')
        document.getElementById(id + '_link').classList.add('active-sidebar-entry')
      }
      activeId = id
    }
  </script>
`

setupDocDir()
copyScripts()
writeStyles()
writeIndexPage()

function writeIndexPage() {
  const page = `<!DOCTYPE html>
<html>
${getHeader()}
<body>
  ${getTopBar({ back: false })}
  <div class="row">
    ${getSideBar()}
    <div class="main">
      <div id="index" class="content active">
      <h1>Welcome to the Lispish playground!</h1>
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
      </ul>
      <p>Have a look at the list of functions to the left. These are what is available in terms of special- and normal expressions.</p>
      <p>For more instruction on how to install and use lispish as a cli or a typescript lib, checkout <a href="https://github.com/mojir/lispish">https://github.com/mojir/lispish</a></p>
      <p/>
      <p>Happy coding!</p>



      </div>
      ${Object.values(reference)
        .map(obj => writeDoc(obj))
        .join('\n')}
      ${getPlayground()}
    </div>
  </div>
  ${getScriptTags()}
</body>
</html>
`
  fs.writeFileSync(path.join(DOC_DIR, `index.html`), page, { encoding: 'utf-8' })
}

function writeDoc(docObj) {
  const { name, longDescription, syntax, linkName } = docObj
  return `    <div id="${linkName}" class="content">
      <h1>${name}</h1>
      <p>${longDescription}</p>
      <label>Syntax</label>
      <pre>${syntax}</pre>
    </div>
`
}

function getSideBar() {
  return `<div class="sidebar">
      <ul>
        ${Object.keys(reference)
          .sort()
          .map(
            key =>
              `<a class="small-pre" onclick="setActive('${reference[key].linkName}')"><li id="${
                reference[key].linkName
              }_link">${escape(key)}</li></a>`,
          )
          .join('\n        ')}
      </ul>
    </div>`
}

function setupDocDir() {
  fs.rmdirSync(DOC_DIR, { recursive: true, force: true })
  fs.mkdirSync(DOC_DIR)
}

function copyScripts() {
  fs.copyFileSync(path.join(__dirname, '../dist/lispish.iife.js'), path.join(DOC_DIR, 'lispish.iife.js'))
}

function escape(str) {
  str = str.replace(/>/g, '&gt;')
  str = str.replace(/</g, '&lt;')
  return str
}

function writeStyles() {
  const styles = `body {
  margin: 0;
  padding: 1rem;
  font-family: verdana;
  background-color: #222222;
  color: #dddddd;
}

a:link, a:visited, a:hover, a:active {
  color: #dddddd;
  text-decoration: none;
  cursor: pointer;
}

#lispish-header:hover {
  text-decoration: underline;
}

.sidebar ul {
  list-style-type: none;
  list-style-position: inside;
  padding: 0;
  margin: 0;
}

.row {
  display: flex;
}

.sidebar {
  flex: 25%;
}

.main {
  padding: 0 2rem;
  flex: 75%;
}

.content {
  display: none;
}

.content.active {
  display: block;
}

.column-third {
  flex: 33.33333%;
}

.column-half {
  flex: 1;
  margin-right: 16px;
}

h4, h2, h1 {
  margin: 0;
  padding: 0;
}

.top-bar {
  background-color: #333333;
  margin-bottom: 3rem;
}

.main-header {
  font-size: 36px;
  font-weight: bold;
}

.small-pre, pre {
  font-family: monospace;
  font-size: 1rem;
}

.sidebar .active-sidebar-entry, .sidebar li:hover.active-sidebar-entry {
  background-color: #444444;
  font-weight: bold;
}

.sidebar li:hover {
  background-color: #333333;
}

.playground {
  margin-top: 9rem;
}

.playground .header {
  font-size: 24px;
  margin-bottom: 1.5rem;
}

textarea {
  outline: none;
  background-color: #333333;
  width: 100%;
  resize: none;
  padding: 0.5rem;
  color: #dddddd;
  font-size: 1rem;
}

.play {
  cursor: pointer;
  color: gray;
}

.play:hover {
  color: white;
}

#output {
  margin-top: 1rem;
  font-family: monospace;
  font-size: 1rem;
}
#output.error {
  color: red;
}

#index pre {
 margin: 0;
 display: inline;
 color: #fdff91;
}

#index a {
  text-decoration: underline;
  color: #fdff91;
 }

`
  fs.writeFileSync(path.join(DOC_DIR, `styles.css`), styles, { encoding: 'utf-8' })
}
