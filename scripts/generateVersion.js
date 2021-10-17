const version = require('../package.json').version
const fs = require('fs')
const path = require('path')

const fileContent = `export const version = \`${version}\`
`

fs.writeFileSync(path.join(__dirname, `../src/version.ts`), fileContent, 'utf-8')
