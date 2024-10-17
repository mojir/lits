const typescript = require('@rollup/plugin-typescript')
const jsonPlugin = require('@rollup/plugin-json')

const plugins = [
  typescript({
    tsconfig: 'tsconfig.playground-builder.json',
  }),
  jsonPlugin(),
]

module.exports = [
  {
    input: 'playground-builder/src/buildPlaygroundSite.ts',
    external: ['node:fs', 'node:path'],
    output: [
      {
        file: 'playground-builder/build/buildPlaygroundSite.js',
        format: 'cjs',
      },
    ],
    plugins,
  },
]
