const typescript = require('@rollup/plugin-typescript')
const jsonPlugin = require('@rollup/plugin-json')

const plugins = [
  typescript({
    tsconfig: 'tsconfig.cli.json',
  }),
  jsonPlugin(),
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
