const typescript = require('@rollup/plugin-typescript')
const jsonPlugin = require('@rollup/plugin-json')
const terser = require('@rollup/plugin-terser')
const pkg = require('./package.json')

const basePlugins = [
  typescript({
    tsconfig: 'tsconfig.json',
  }),
  jsonPlugin(),
]

const plugins = [
  ...basePlugins,
  terser(),
]

/**
 * Rollup plugin to strip `docs` fields from built-in expressions.
 * Used for the minimal bundle to reduce size.
 * Operates on the final JavaScript output via renderChunk.
 */
function stripDocsPlugin() {
  return {
    name: 'strip-docs',
    renderChunk(code) {
      let result = code

      // Helper: find the end of a balanced brace block starting at `{`
      function findBalancedBraceEnd(str, start) {
        let depth = 0
        for (let i = start; i < str.length; i++) {
          if (str[i] === '{') {
            depth++
          }
          else if (str[i] === '}') {
            depth--
            if (depth === 0) {
              return i
            }
          }
        }
        return -1
      }

      // Helper: remove all occurrences of a pattern followed by a balanced `{ ... }` block
      function stripBalancedBlocks(str, pattern) {
        let result = str
        // eslint-disable-next-line no-cond-assign
        for (let match; (match = pattern.exec(result)) !== null;) {
          const braceStart = result.indexOf('{', match.index + match[0].length - 1)
          if (braceStart === -1) {
            break
          }
          const braceEnd = findBalancedBraceEnd(result, braceStart)
          if (braceEnd === -1) {
            break
          }
          result = result.slice(0, match.index) + result.slice(braceEnd + 1)
          pattern.lastIndex = 0 // reset since string changed
        }
        return result
      }

      // 1. Remove inline `docs: { ... }` property (with balanced braces)
      result = stripBalancedBlocks(result, /docs:\s*(?=\{)/g)

      // 2. Remove `docs: variableName` (variable reference, not inline object)
      result = result.replace(/docs:\s*[a-zA-Z_$][\w$]*/g, '')

      // 3. Remove standalone docs variable declarations: `var/const/let docsXxx = { ... };`
      result = stripBalancedBlocks(result, /(?:var|const|let)\s+\w*[Dd]ocs\w*\s*=\s*(?=\{)/g)
      // Clean up trailing semicolons left behind
      result = result.replace(/^\s*;\s*$/gm, '')

      // 4. Remove shorthand `docs` property in object literals (bare `docs` as object key)
      result = result.replace(/(?<=\W)docs\s*(?=[,}])/g, '')

      // 5. Clean up comma/whitespace artifacts from removals
      result = result.replace(/,(\s*),/g, ',$1') // double commas → single
      result = result.replace(/\{(\s*),/g, '{$1') // leading comma after {
      result = result.replace(/,(\s*),/g, ',$1') // second pass for triple commas

      if (result !== code)
        return { code: result, map: null }
      return null
    },
  }
}

// Strip docs first, then minify
const pluginsMinimal = [
  ...basePlugins,
  stripDocsPlugin(),
  terser(),
]

const modules = ['assert', 'grid', 'random', 'vector', 'linearAlgebra', 'matrix', 'numberTheory', 'stringUtils', 'collectionUtils']

module.exports = [
  // Minimal bundle (core only, no modules, docs stripped)
  {
    input: 'src/index.ts',
    output: [
      {
        file: pkg.module,
        format: 'esm',
        sourcemap: true,
      },
      {
        file: pkg.main,
        format: 'cjs',
        sourcemap: true,
      },
      {
        file: pkg.iife,
        format: 'iife',
        sourcemap: true,
        name: 'Lits',
      },
    ],
    plugins: pluginsMinimal,
  },
  // Full bundle (core + all modules + docs + reference data)
  {
    input: 'src/full.ts',
    output: [
      {
        file: 'dist/full.esm.js',
        format: 'esm',
        sourcemap: true,
      },
      {
        file: 'dist/full.js',
        format: 'cjs',
        sourcemap: true,
      },
    ],
    plugins,
  },
  // Individual module bundles
  ...modules.map(ns => ({
    input: `src/modules/${ns}.ts`,
    output: [
      {
        file: `dist/modules/${ns}.esm.js`,
        format: 'esm',
        sourcemap: true,
      },
      {
        file: `dist/modules/${ns}.js`,
        format: 'cjs',
        sourcemap: true,
      },
    ],
    plugins,
  })),
  // Test framework bundle
  {
    input: 'src/testFramework/index.ts',
    external: ['node:fs', 'node:path'],
    output: [
      {
        file: 'dist/testFramework.esm.js',
        format: 'esm',
        sourcemap: true,
      },
      {
        file: 'dist/testFramework.js',
        format: 'cjs',
        sourcemap: true,
      },
    ],
    plugins,
  },
]
