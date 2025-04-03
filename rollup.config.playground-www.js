const resolve = require('@rollup/plugin-node-resolve')
const typescript = require('@rollup/plugin-typescript')
const jsonPlugin = require('@rollup/plugin-json')

const plugins = [
  typescript({
    tsconfig: 'tsconfig.playground-www.json',
  }),
  jsonPlugin(),
  resolve({
    // options to customize how modules are resolved
    extensions: ['.js', '.ts'], // add file extensions you're using
  }),
]

module.exports = [
  {
    onwarn(warning, warn) {
      // suppress eval warnings
      if (warning.code === 'EVAL')
        return

      warn(warning)
    },
    input: 'playground-www/src/playground.ts',
    output: [
      {
        file: 'playground-www/build/playground.js',
        format: 'iife',
        name: 'Playground',
      },
    ],
    plugins,
  },
]
