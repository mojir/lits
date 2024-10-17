const typescript = require('@rollup/plugin-typescript')
const jsonPlugin = require('@rollup/plugin-json')
const pkg = require('./package.json')

const input = 'src/index.ts'

const plugins = [
  typescript({
    tsconfig: 'tsconfig.json',
  }),
  jsonPlugin(),
]

module.exports = [
  {
    input,
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
    plugins,
  },
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
