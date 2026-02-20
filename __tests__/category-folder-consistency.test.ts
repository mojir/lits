import { describe, expect, it } from 'vitest'
import { allBuiltinModules } from '../src/allModules'
import { moduleReference } from '../reference'
import type { FunctionReference } from '../reference/index'

// Enforce: category must be the same as the folder name for all module functions

describe('category matches folder name for all module functions', () => {
  for (const module of allBuiltinModules) {
    // Get the folder name (last part of the module path)
    // e.g. src/builtin/modules/linear-algebra/index.ts → linear-algebra
    const folderName = module.name
    for (const fnName of Object.keys(module.functions)) {
      const qualified = `${module.name}.${fnName}`
      const ref = moduleReference[qualified as keyof typeof moduleReference] as FunctionReference | undefined
      if (!ref || typeof ref.category !== 'string')
        continue
      it(`${qualified} category === folder name (${folderName})`, () => {
        expect(ref.category).toBe(folderName)
      })
    }
  }
})
