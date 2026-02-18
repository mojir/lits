/**
 * Phase 3 Migration Script: Generate namespace docs files
 *
 * For each namespace, reads the legacy reference data and generates
 * a `docs.ts` file containing a Record<string, FunctionDocs> with
 * all the docs for that namespace's functions.
 *
 * Then each namespace's index.ts needs a small wire-up loop to attach
 * the docs to the expression objects.
 *
 * Usage:
 *   npx tsx scripts/refactor/migrate-namespace-docs.ts [--dry-run]
 */

import * as fs from 'node:fs'
import * as path from 'node:path'

const DRY_RUN = process.argv.includes('--dry-run')
const ROOT = path.resolve(new URL('.', import.meta.url).pathname, '../..')

interface NamespaceConfig {
  name: string
  category: string
  implDir: string // directory of the namespace implementation
  refFile: string
  refExportName: string
}

const namespaces: NamespaceConfig[] = [
  {
    name: 'Assert',
    category: 'Assert',
    implDir: 'src/builtin/namespaces/assert',
    refFile: 'reference/categories/assert/index.ts',
    refExportName: 'assertReference',
  },
  {
    name: 'Grid',
    category: 'Grid',
    implDir: 'src/builtin/namespaces/grid',
    refFile: 'reference/categories/grid/index.ts',
    refExportName: 'gridReference',
  },
  {
    name: 'Random',
    category: 'Random',
    implDir: 'src/builtin/namespaces/random',
    refFile: 'reference/categories/random.ts',
    refExportName: 'randomReference',
  },
  {
    name: 'Vector',
    category: 'Vector',
    implDir: 'src/builtin/namespaces/vector',
    refFile: 'reference/categories/vector/index.ts',
    refExportName: 'vectorReference',
  },
  {
    name: 'LinAlg',
    category: 'Linear Algebra',
    implDir: 'src/builtin/namespaces/linearAlgebra',
    refFile: 'reference/categories/linearAlgebra/index.ts',
    refExportName: 'linAlgReference',
  },
  {
    name: 'Matrix',
    category: 'Matrix',
    implDir: 'src/builtin/namespaces/matrix',
    refFile: 'reference/categories/matrix/index.ts',
    refExportName: 'matrixReference',
  },
  {
    name: 'Number-Theory',
    category: 'Number Theory',
    implDir: 'src/builtin/namespaces/numberTheory',
    refFile: 'reference/categories/numberTheory/index.ts',
    refExportName: 'numberTheoryReference',
  },
]

// --- Reference loading ---

async function loadReferenceData(config: NamespaceConfig): Promise<Map<string, Record<string, unknown>>> {
  const fullPath = path.join(ROOT, config.refFile)
  const mod = await import(fullPath)
  const refData = mod[config.refExportName] as Record<string, Record<string, unknown>>

  const result = new Map<string, Record<string, unknown>>()
  for (const [qualifiedKey, entry] of Object.entries(refData)) {
    const dotIndex = qualifiedKey.indexOf('.')
    const unqualifiedKey = dotIndex >= 0 ? qualifiedKey.slice(dotIndex + 1) : qualifiedKey
    result.set(unqualifiedKey, entry)
  }
  return result
}

// --- Generate docs.ts file content ---

function generateDocsFile(entries: Map<string, Record<string, unknown>>, category: string): string {
  const lines: string[] = []
  lines.push(`import type { FunctionDocs } from '../../interface'`)
  lines.push(``)
  lines.push(`export const namespaceDocs: Record<string, FunctionDocs> = {`)

  for (const [key, ref] of entries) {
    const safeKey = /^[a-z_$][a-z0-9_$]*$/i.test(key) ? key : `'${key}'`
    lines.push(`  ${safeKey}: {`)
    lines.push(`    category: ${quote(category)},`)
    lines.push(`    description: ${formatString(ref.description as string)},`)
    lines.push(`    returns: ${formatValue(ref.returns, 4)},`)
    if (ref.args != null) {
      lines.push(`    args: ${formatValue(ref.args, 4)},`)
    }
    else {
      lines.push(`    args: {},`)
    }
    if (ref.variants != null) {
      lines.push(`    variants: ${formatValue(ref.variants, 4)},`)
    }
    else {
      lines.push(`    variants: [],`)
    }
    lines.push(`    examples: ${formatValue(ref.examples, 4)},`)
    if (ref.seeAlso != null) {
      lines.push(`    seeAlso: ${formatValue(ref.seeAlso, 4)},`)
    }
    if (ref.noOperatorDocumentation) {
      lines.push(`    hideOperatorForm: true,`)
    }
    lines.push(`  },`)
  }

  lines.push(`}`)
  lines.push(``)
  return lines.join('\n')
}

function quote(s: string): string {
  if (s.includes("'")) return `'${s.replace(/'/g, "\\'")}'`
  return `'${s}'`
}

function formatString(s: string): string {
  if (s.includes('\n') || s.includes('`')) {
    // Use JSON.stringify for safety
    return JSON.stringify(s)
  }
  return JSON.stringify(s)
}

function formatValue(value: unknown, indent: number): string {
  const json = JSON.stringify(value, null, 2)
  const lines = json.split('\n')
  if (lines.length === 1) return json
  const pad = ' '.repeat(indent)
  return lines.map((line, i) => (i === 0 ? line : pad + line)).join('\n')
}

// --- Main ---

async function main() {
  let totalEntries = 0

  for (const config of namespaces) {
    console.log(`\n=== ${config.name} (category: ${config.category}) ===`)

    const refMap = await loadReferenceData(config)
    console.log(`  Reference entries: ${refMap.size}`)
    totalEntries += refMap.size

    const docsContent = generateDocsFile(refMap, config.category)
    const docsPath = path.join(ROOT, config.implDir, 'docs.ts')

    if (DRY_RUN) {
      console.log(`  WOULD write ${docsPath}`)
      console.log(`  Preview (first 20 lines):`)
      docsContent.split('\n').slice(0, 20).forEach(l => console.log(`    ${l}`))
    }
    else {
      fs.writeFileSync(docsPath, docsContent)
      console.log(`  Wrote ${docsPath}`)
    }
  }

  console.log(`\n=== Summary ===`)
  console.log(`Namespaces: ${namespaces.length}`)
  console.log(`Total doc entries: ${totalEntries}`)
  if (DRY_RUN) {
    console.log(`\n(DRY RUN — no files were written)`)
  }
  else {
    console.log(`\nDocs files written. Next steps:`)
    console.log(`1. In each namespace index.ts, add:`)
    console.log(`   import { namespaceDocs } from './docs'`)
    console.log(`   // After building the functions record:`)
    console.log(`   for (const [key, docs] of Object.entries(namespaceDocs)) {`)
    console.log(`     if (functions[key]) functions[key].docs = docs`)
    console.log(`   }`)
    console.log(`2. Update reference/index.ts to use docsToReference() for namespaces`)
    console.log(`3. Run npm run check`)
  }
}

main().catch(console.error)
