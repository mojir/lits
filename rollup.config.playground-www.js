const typescript = require('@rollup/plugin-typescript')
const jsonPlugin = require('@rollup/plugin-json')

const plugins = [
  typescript({
    tsconfig: 'tsconfig.playground-www.json',
  }),
  jsonPlugin(),
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
