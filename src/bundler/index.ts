import fs from 'node:fs'
import path from 'node:path'
import type { LitsBundle } from './interface'

const builtinModuleNames = new Set([
  'assert',
  'grid',
  'random',
  'vector',
  'linear-algebra',
  'matrix',
  'number-theory',
  'math',
  'functional',
  'string',
  'collection',
  'sequence',
  'bitwise',
])

/**
 * Regex to match `import("...")` or `import('...')` calls in Lits source.
 * Captures the file path inside the quotes.
 */
const fileImportPattern = /import\(\s*"([^"]+)"\s*\)|import\(\s*'([^']+)'\s*\)/g

/**
 * Bundles a Lits entry file and all its file imports into a LitsBundle.
 *
 * Resolves all `import("./path/to/file.lits")` calls recursively,
 * deduplicates, detects circular dependencies, topologically sorts,
 * and rewrites file imports to bare symbol imports.
 */
export function bundle(entryPath: string): LitsBundle {
  const absoluteEntryPath = path.resolve(entryPath)
  const entryDir = path.dirname(absoluteEntryPath)

  // Map from absolute file path → source code
  const fileSources = new Map<string, string>()
  // Map from absolute file path → canonical module name
  const canonicalNames = new Map<string, string>()
  // Adjacency list: file → set of files it imports
  const dependencies = new Map<string, Set<string>>()

  // Phase 1: Resolve all file imports recursively
  resolveFile(absoluteEntryPath, [])

  // Phase 2: Build canonical names
  buildCanonicalNames(entryDir)

  // Phase 3: Topological sort (exclude entry file — it becomes the program)
  const sorted = topologicalSort(absoluteEntryPath)

  // Phase 4: Rewrite imports and build bundle
  const fileModules: [string, string][] = sorted.map((filePath) => {
    const source = fileSources.get(filePath)!
    const rewritten = rewriteImports(source, filePath)
    return [canonicalNames.get(filePath)!, rewritten]
  })

  const entrySource = fileSources.get(absoluteEntryPath)!
  const program = rewriteImports(entrySource, absoluteEntryPath)

  return { program, fileModules }

  // --- Helper functions (closures over the maps above) ---

  function resolveFile(absoluteFilePath: string, stack: string[]): void {
    // Circular dependency detection
    if (stack.includes(absoluteFilePath)) {
      const cycle = [...stack.slice(stack.indexOf(absoluteFilePath)), absoluteFilePath]
      throw new Error(`Circular dependency detected: ${cycle.join(' → ')}`)
    }

    // Already resolved (deduplication)
    if (fileSources.has(absoluteFilePath)) {
      return
    }

    if (!fs.existsSync(absoluteFilePath)) {
      throw new Error(`File not found: ${absoluteFilePath}`)
    }

    const source = fs.readFileSync(absoluteFilePath, 'utf-8')
    fileSources.set(absoluteFilePath, source)

    const deps = new Set<string>()
    dependencies.set(absoluteFilePath, deps)

    const dir = path.dirname(absoluteFilePath)

    for (const match of source.matchAll(fileImportPattern)) {
      const importPath = (match[1] ?? match[2])!
      const resolvedPath = path.resolve(dir, importPath)

      deps.add(resolvedPath)
      resolveFile(resolvedPath, [...stack, absoluteFilePath])
    }
  }

  function buildCanonicalNames(entryDirectory: string): void {
    const usedNames = new Set<string>()

    for (const absoluteFilePath of fileSources.keys()) {
      // Skip the entry file — it doesn't need a canonical name
      if (absoluteFilePath === absoluteEntryPath) {
        continue
      }

      let name = deriveCanonicalName(absoluteFilePath, entryDirectory)

      // Resolve collisions with builtin modules
      while (builtinModuleNames.has(name) || usedNames.has(name)) {
        name = `_${name}`
      }

      usedNames.add(name)
      canonicalNames.set(absoluteFilePath, name)
    }
  }

  function deriveCanonicalName(absoluteFilePath: string, entryDirectory: string): string {
    const relativePath = path.relative(entryDirectory, absoluteFilePath)

    // If the file is under the entry directory (no leading ..)
    if (!relativePath.startsWith('..')) {
      // Strip .lits extension
      return stripExtension(relativePath)
    }

    // File is outside the entry directory — use last N path segments
    const segments = absoluteFilePath.split(path.sep)
    // Use last 2 segments (directory + filename) for readability
    const fallback = segments.slice(-2).join('/')
    return stripExtension(fallback)
  }

  function stripExtension(filePath: string): string {
    if (filePath.endsWith('.lits')) {
      return filePath.slice(0, -5)
    }
    return filePath
  }

  function topologicalSort(entryFilePath: string): string[] {
    const result: string[] = []
    const visited = new Set<string>()
    const visiting = new Set<string>()

    function visit(filePath: string): void {
      if (visited.has(filePath)) {
        return
      }
      /* v8 ignore next 3 */
      if (visiting.has(filePath))
        throw new Error(`Circular dependency detected during topological sort: ${filePath}`)

      visiting.add(filePath)

      const deps = dependencies.get(filePath)
      if (deps) {
        for (const dep of deps) {
          visit(dep)
        }
      }

      visiting.delete(filePath)
      visited.add(filePath)

      // Don't add the entry file — it becomes the program
      if (filePath !== entryFilePath) {
        result.push(filePath)
      }
    }

    // Start from entry file to ensure all reachable files are visited
    visit(entryFilePath)

    return result
  }

  function rewriteImports(source: string, sourceFilePath: string): string {
    const dir = path.dirname(sourceFilePath)

    return source.replace(fileImportPattern, (_fullMatch, doubleQuoted: string | undefined, singleQuoted: string | undefined) => {
      const importPath = (doubleQuoted ?? singleQuoted)!
      const resolvedPath = path.resolve(dir, importPath)
      const canonicalName = canonicalNames.get(resolvedPath)

      /* v8 ignore next 3 */
      if (!canonicalName)
        throw new Error(`No canonical name for: ${resolvedPath}`)

      return `import(${canonicalName})`
    })
  }
}
