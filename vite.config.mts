import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    coverage: {
      exclude: [
        '**/[.]**',
        '__tests__/**',
        '**/*.test.ts',
        'playground-builder/**',
        'playground-www/**',
        'reference/**',
        'dist/**',
        'node_modules/**',
        'build/**',
        'cli/**',
        'docs/**',
        'common/**',
        'scripts/**',
        '**/interface.ts',
        '**/types.ts',
        'src/index.ts',
        'src/full.ts',
      ],
    },
  },
})
