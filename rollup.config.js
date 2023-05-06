import typescript from 'rollup-plugin-typescript2'
// import gzipPlugin from 'rollup-plugin-gzip'
// import { terser } from 'rollup-plugin-terser'
import pkg from './package.json'

const input = 'src/index.ts'

const plugins = [
  typescript({
    typescript: require('typescript'),
  }),
  // terser(),
  // gzipPlugin(),
]

export default [
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
    external: ['fs', 'path'],
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
