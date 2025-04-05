import { vi } from 'vitest'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    setupFiles: ['./vitest.setup.mjs'],
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
        '**/interface.ts',
        '**/types.ts',
      ],
    },
  },
})
