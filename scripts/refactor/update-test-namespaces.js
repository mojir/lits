/**
 * Script to update test files to explicitly pass namespaces to Lits constructor.
 * Part of Phase 5 refactoring: making Lits default to no namespaces.
 */
const fs = require('fs')
const path = require('path')
const { execSync } = require('child_process')

// Map namespace directory names to their import info
const namespaceMap = {
  assert: { varName: 'assertNamespace', importName: 'assertNamespace', importFrom: 'assert' },
  grid: { varName: 'gridNamespace', importName: 'gridNamespace', importFrom: 'grid' },
  random: { varName: 'randomNamespace', importName: 'randomNamespace', importFrom: 'random' },
  vector: { varName: 'vectorNamespace', importName: 'vectorNamespace', importFrom: 'vector' },
  linearAlgebra: { varName: 'linearAlgebraNamespace', importName: 'linearAlgebraNamespace', importFrom: 'linearAlgebra' },
  matrix: { varName: 'matrixNamespace', importName: 'matrixNamespace', importFrom: 'matrix' },
  numberTheory: { varName: 'numberTheoryNamespace', importName: 'numberTheoryNamespace', importFrom: 'numberTheory' },
}

const ROOT = path.join(__dirname, '..', '..')
let changeCount = 0

function getNamespaceForFile(filePath) {
  const rel = path.relative(ROOT, filePath)
  for (const [dirName, nsInfo] of Object.entries(namespaceMap)) {
    if (rel.startsWith(`src/builtin/namespaces/${dirName}/`)) {
      return nsInfo
    }
  }
  return null
}

function computeRelativeImport(fromFile, nsInfo) {
  const fromDir = path.dirname(fromFile)
  const nsIndexDir = path.join(ROOT, 'src', 'builtin', 'namespaces', nsInfo.importFrom)
  let rel = path.relative(fromDir, nsIndexDir)
  if (!rel.startsWith('.')) rel = './' + rel
  return rel
}

function updateNamespaceTestFile(filePath) {
  const nsInfo = getNamespaceForFile(filePath)
  if (!nsInfo) return false

  let content = fs.readFileSync(filePath, 'utf8')
  const importPath = computeRelativeImport(filePath, nsInfo)

  // Check if already updated
  if (content.includes(nsInfo.importName)) return false

  // Add namespace import after the Lits import line
  const litsImportRegex = /(import\s+\{[^}]*Lits[^}]*\}\s+from\s+['"][^'"]+['"])/
  const match = content.match(litsImportRegex)
  if (!match) {
    console.log(`  SKIP (no Lits import): ${filePath}`)
    return false
  }

  content = content.replace(
    litsImportRegex,
    `$1\nimport { ${nsInfo.importName} } from '${importPath}'`
  )

  // Replace `new Lits()` with `new Lits({ namespaces: [namespace] })`
  content = content.replace(
    /new Lits\(\)/g,
    `new Lits({ namespaces: [${nsInfo.importName}] })`
  )

  // Replace `new Lits({ debug: true })` with `new Lits({ debug: true, namespaces: [namespace] })`
  content = content.replace(
    /new Lits\(\{\s*debug:\s*true\s*\}\)/g,
    `new Lits({ debug: true, namespaces: [${nsInfo.importName}] })`
  )

  fs.writeFileSync(filePath, content)
  changeCount++
  console.log(`  Updated: ${filePath}`)
  return true
}

function updateSpecialFiles() {
  // 1. src/testFramework/index.ts — needs allBuiltinNamespaces
  const testFrameworkPath = path.join(ROOT, 'src', 'testFramework', 'index.ts')
  let content = fs.readFileSync(testFrameworkPath, 'utf8')
  if (!content.includes('allBuiltinNamespaces')) {
    content = content.replace(
      "import { Lits } from '../Lits/Lits'",
      "import { Lits } from '../Lits/Lits'\nimport { allBuiltinNamespaces } from '../allNamespaces'"
    )
    content = content.replace(
      'new Lits({ debug: true })',
      'new Lits({ debug: true, namespaces: allBuiltinNamespaces })'
    )
    fs.writeFileSync(testFrameworkPath, content)
    changeCount++
    console.log(`  Updated: ${testFrameworkPath}`)
  }

  // 2. __tests__/destructuring.test.ts — needs gridNamespace
  const destructuringPath = path.join(ROOT, '__tests__', 'destructuring.test.ts')
  content = fs.readFileSync(destructuringPath, 'utf8')
  if (!content.includes('gridNamespace')) {
    content = content.replace(
      "import { Lits } from '../src/Lits/Lits'",
      "import { Lits } from '../src/Lits/Lits'\nimport { gridNamespace } from '../src/builtin/namespaces/grid'"
    )
    content = content.replace(
      /new Lits\(\)/g,
      'new Lits({ namespaces: [gridNamespace] })'
    )
    fs.writeFileSync(destructuringPath, content)
    changeCount++
    console.log(`  Updated: ${destructuringPath}`)
  }

  // 3. __tests__/builtin/normalExpressions/array.test.ts — needs vectorNamespace
  const arrayTestPath = path.join(ROOT, '__tests__', 'builtin', 'normalExpressions', 'array.test.ts')
  content = fs.readFileSync(arrayTestPath, 'utf8')
  if (!content.includes('vectorNamespace')) {
    content = content.replace(
      "import { Lits } from '../../../src/Lits/Lits'",
      "import { Lits } from '../../../src/Lits/Lits'\nimport { vectorNamespace } from '../../../src/builtin/namespaces/vector'"
    )
    content = content.replace(
      /new Lits\(\)/g,
      'new Lits({ namespaces: [vectorNamespace] })'
    )
    fs.writeFileSync(arrayTestPath, content)
    changeCount++
    console.log(`  Updated: ${arrayTestPath}`)
  }

  // 4. src/builtin/namespaces/registry.test.ts — needs rewriting
  const registryTestPath = path.join(ROOT, 'src', 'builtin', 'namespaces', 'registry.test.ts')
  content = fs.readFileSync(registryTestPath, 'utf8')
  if (!content.includes('allBuiltinNamespaces')) {
    content = content.replace(
      "import { Lits } from '../../Lits/Lits'",
      "import { Lits } from '../../Lits/Lits'\nimport { allBuiltinNamespaces } from '../../allNamespaces'"
    )
    // Replace new Lits() with new Lits({ namespaces: allBuiltinNamespaces }) where tests expect all namespaces
    // But also leave the first describe block to test empty default
    fs.writeFileSync(registryTestPath, content)
    changeCount++
    console.log(`  Updated (needs manual review): ${registryTestPath}`)
  }
}

// Find all namespace test files
function findTestFiles(dir) {
  const files = []
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const fullPath = path.join(dir, entry.name)
    if (entry.isDirectory()) {
      files.push(...findTestFiles(fullPath))
    } else if (entry.name.endsWith('.test.ts')) {
      files.push(fullPath)
    }
  }
  return files
}

console.log('Updating namespace test files...')
const namespacesDir = path.join(ROOT, 'src', 'builtin', 'namespaces')
for (const [nsDir] of Object.entries(namespaceMap)) {
  const nsPath = path.join(namespacesDir, nsDir)
  if (fs.existsSync(nsPath)) {
    const testFiles = findTestFiles(nsPath)
    for (const testFile of testFiles) {
      updateNamespaceTestFile(testFile)
    }
  }
}

console.log('\nUpdating special files...')
updateSpecialFiles()

console.log(`\nDone! Updated ${changeCount} files.`)
