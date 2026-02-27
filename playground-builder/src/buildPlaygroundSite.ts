import path from 'node:path'
import fs from 'node:fs'
import { getAllDocumentationItems } from './components/functionDocumentation'
import { getSearchDialog } from './components/searchDialog/searchDialog'
import { randomNumbers } from './randomNumbers'
import { getStartPage } from './components/startPage'
import { getExamplePage } from './components/examplePage'
import { getModulesPage } from './components/modulesPage'
import { getCorePage } from './components/corePage'
import { getAllTutorialPages } from './components/tutorials'
import { getPlayground } from './components/playground'
import { getSideBar } from './components/sideBar'
import { allSearchResultEntries } from './allSearchResultEntries'
import { styles } from './styles'

const DOC_DIR = path.resolve(__dirname, '../../docs')
setupPredictability()
setupDocDir()
copyAssets()
writeIndexPage()

function writeIndexPage() {
  const page = `<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Playground</title>
    <link rel='shortcut icon' type='image/x-icon' href='favicon.ico' />
    <meta name="description" content="A reference and a playground for Lits">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta charset="UTF-8">
    <link rel="stylesheet" href="styles.css">
  </head>

  <body>
    <div id="wrapper" ${styles('hidden')}>
      <main id="main-panel" class="fancy-scroll">
        ${getStartPage()}
        ${getExamplePage()}
        ${getAllTutorialPages()}
        ${getCorePage()}
        ${getModulesPage()}
        ${getAllDocumentationItems()}
      </main>
      <div id="resize-sidebar" style="position: fixed; width: 5px; cursor: col-resize; background-color: rgb(82 82 82); top: 0; z-index: 10;"></div>
      ${getSideBar()}
      ${getPlayground()}
    </div>
    ${getSearchDialog()}

    <script src='playground.js'></script>
    <script>
      window.Playground.allSearchResultEntries = JSON.parse(decodeURIComponent(atob('${btoa(encodeURIComponent(JSON.stringify(allSearchResultEntries)))}')))
    </script>
  </body>
</html>
`
  fs.writeFileSync(path.join(DOC_DIR, 'index.html'), page, { encoding: 'utf-8' })
}

function setupDocDir() {
  fs.rmSync(DOC_DIR, { recursive: true, force: true })
  fs.mkdirSync(DOC_DIR)
}

function copyAssets() {
  fs.cpSync(path.join(__dirname, '../../playground-www/public/'), path.join(DOC_DIR), { recursive: true })
  fs.copyFileSync(path.join(__dirname, '../../playground-www/build/playground.js'), path.join(DOC_DIR, 'playground.js'))
}

function setupPredictability() {
  let i = 0
  Math.random = () => {
    const result = randomNumbers[i]!
    i = (i + 1) % randomNumbers.length
    return result
  }

  Date.now = () => 1712145842007
}
