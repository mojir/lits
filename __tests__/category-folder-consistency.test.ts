import { describe, expect, it } from 'vitest'
import { allBuiltinModules } from '../src/allModules'
import { moduleReference } from '../reference'
import type { FunctionReference } from '../reference/index'

// Enforce: category must match the expected category for all module functions
// Module name → category mapping (most modules use name as category)
const moduleCategoryMap: Record<string, string> = {
}
function getExpectedCategory(moduleName: string): string {
  return moduleCategoryMap[moduleName] ?? moduleName
}

describe('category matches expected category for all module functions', () => {
  for (const module of allBuiltinModules) {
    const expectedCategory = getExpectedCategory(module.name)
    for (const fnName of Object.keys(module.functions)) {
      const qualified = `${module.name}.${fnName}`
      const ref = moduleReference[qualified as keyof typeof moduleReference] as FunctionReference | undefined
      if (!ref || typeof ref.category !== 'string')
        continue
      it(`${qualified} category === ${expectedCategory}`, () => {
        expect(ref.category).toBe(expectedCategory)
      })
    }
  }
})
