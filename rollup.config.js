const typescript = require('@rollup/plugin-typescript')
const jsonPlugin = require('@rollup/plugin-json')
const pkg = require('./package.json')

const plugins = [
  typescript({
    tsconfig: 'tsconfig.json',
  }),
  jsonPlugin(),
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
      // 1. Remove inline docs objects: `docs: { ... }` with up to 2 levels of nested braces
      result = result.replace(/,?\s*docs:\s*\{(?:[^{}]|\{(?:[^{}]|\{[^{}]*\})*\})*\}/g, '')
      // 2. Remove variable-reference docs: `docs: variableName,` or `docs: variableName\n`
      result = result.replace(/,?\s*docs:\s*[a-zA-Z_$][\w$]*\s*,?/g, () => {
        // Preserve trailing comma if there was one before
        return ''
      })
      // 3. Remove standalone docs variable declarations: `var docsName = { ... };`
      result = result.replace(/var\s+\w+Docs\s*=\s*\{(?:[^{}]|\{(?:[^{}]|\{[^{}]*\})*\})*\};\s*/g, '')
      result = result.replace(/var\s+docs\$\w+\s*=\s*\{(?:[^{}]|\{(?:[^{}]|\{[^{}]*\})*\})*\};\s*/g, '')
      if (result !== code)
        return { code: result, map: null }
      return null
    },
  }
}

const pluginsMinimal = [
  ...plugins,
  stripDocsPlugin(),
]

const namespaces = ['assert', 'grid', 'random', 'vector', 'linearAlgebra', 'matrix', 'numberTheory']

module.exports = [
  // Minimal bundle (core only, no namespaces, docs stripped)
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
  // Full bundle (core + all namespaces + docs + reference data)
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
  // Individual namespace bundles
  ...namespaces.map(ns => ({
    input: `src/namespaces/${ns}.ts`,
    output: [
      {
        file: `dist/namespaces/${ns}.esm.js`,
        format: 'esm',
        sourcemap: true,
      },
      {
        file: `dist/namespaces/${ns}.js`,
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
