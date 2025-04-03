const resolve = require('@rollup/plugin-node-resolve')
const typescript = require('@rollup/plugin-typescript')
const jsonPlugin = require('@rollup/plugin-json')

const plugins = [
  typescript({
    tsconfig: 'tsconfig.cli.json',
  }),
  jsonPlugin(),
  resolve({
    // options to customize how modules are resolved
    extensions: ['.js', '.ts'], // add file extensions you're using
  }),
]

module.exports = [
  {
    input: 'cli/src/cli.ts',
    external: ['node:fs', 'node:path', 'node:os', 'node:readline'],
    output: [
      {
        file: 'dist/cli/cli.js',
        format: 'cjs',
      },
    ],
    plugins,
  },
]
