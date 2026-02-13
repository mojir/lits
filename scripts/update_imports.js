const fs = require('node:fs')
const path = require('node:path')

const namespaceDirs = [
  'reference/categories/vector',
  'reference/categories/matrix',
  'reference/categories/grid',
  'reference/categories/linearAlgebra',
  'reference/categories/numberTheory',
  'reference/categories/random',
]

let totalUpdated = 0

for (const dir of namespaceDirs) {
  const fullDir = path.join('/Users/albert.mojir/mojir/lits', dir)
  if (!fs.existsSync(fullDir))
    continue

  const files = fs.readdirSync(fullDir).filter(f => f.endsWith('.ts'))

  for (const file of files) {
    const filePath = path.join(fullDir, file)
    let content = fs.readFileSync(filePath, 'utf8')
    let modified = false

    // Fix namespace name: nth -> TEMP-nth
    if (content.includes('import("nth")')) {
      content = content.replace(/import\("nth"\)/g, 'import("TEMP-nth")')
      content = content.replace(/let nth = /g, 'let nt = ')
      content = content.replace(/\\nnth\./g, '\\nnt.')
      modified = true
      totalUpdated++
    }

    // Fix vec.min -> vec.TEMP-min
    if (content.includes('vec.min(') || content.includes('{ min }')) {
      content = content.replace(/vec\.min\(/g, 'vec.TEMP-min(')
      content = content.replace(/\{ min \}/g, '{ "TEMP-min": tempMin }')
      content = content.replace(/\\nmin\(/g, '\\ntempMin(')
      modified = true
      totalUpdated++
    }

    // Fix vec.max -> vec.TEMP-max
    if (content.includes('vec.max(') || content.includes('{ max }')) {
      content = content.replace(/vec\.max\(/g, 'vec.TEMP-max(')
      content = content.replace(/\{ max \}/g, '{ "TEMP-max": tempMax }')
      content = content.replace(/\\nmax\(/g, '\\ntempMax(')
      modified = true
      totalUpdated++
    }

    // Fix grid.map -> grid.TEMP-map
    if (content.includes('grid.map(')) {
      content = content.replace(/grid\.map\(/g, 'grid.TEMP-map(')
      modified = true
      totalUpdated++
    }

    // Fix grid.reduce -> grid.TEMP-reduce
    if (content.includes('grid.reduce(')) {
      content = content.replace(/grid\.reduce\(/g, 'grid.TEMP-reduce(')
      modified = true
      totalUpdated++
    }

    if (modified) {
      fs.writeFileSync(filePath, content)
      console.log('Updated:', filePath)
    }
  }
}

console.log('\nTotal updates:', totalUpdated)
