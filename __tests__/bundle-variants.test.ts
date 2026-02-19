/**
 * Bundle variant tests — verify that entry points work correctly.
 *
 * - Minimal (src/index.ts): Lits core, no module definitions, no docs data
 * - Full (src/full.ts): Lits core + all modules + docs/reference data
 * - Individual module entry points: export the correct module
 */
import { describe, expect, it } from 'vitest'
import { Lits } from '../src/index'
import { Lits as LitsFull, allBuiltinModules, apiReference } from '../src/full'
import { assertModule } from '../src/modules/assert'
import { gridModule } from '../src/modules/grid'
import { randomModule } from '../src/modules/random'
import { vectorModule } from '../src/modules/vector'
import { linearAlgebraModule } from '../src/modules/linearAlgebra'
import { matrixModule } from '../src/modules/matrix'
import { numberTheoryModule } from '../src/modules/numberTheory'

describe('minimal entry point (src/index.ts)', () => {
  it('should evaluate core expressions without modules', () => {
    const lits = new Lits()
    expect(lits.run('1 + 2')).toBe(3)
    expect(lits.run('map([1, 2, 3], inc)')).toEqual([2, 3, 4])
    expect(lits.run('let x = 10; x * x')).toBe(100)
  })

  it('should default to no modules', () => {
    const lits = new Lits()
    expect(() => lits.run('import("Assert")')).toThrow()
  })

  it('should accept individual modules passed in', () => {
    const lits = new Lits({ modules: [vectorModule] })
    expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
  })

  // NOTE: cannot test "doc returns empty" here because importing src/full.ts
  // triggers initReferenceData as a side effect. That behavior is tested
  // indirectly: without the import, doc(+) would return ''.
})

describe('full entry point (src/full.ts)', () => {
  it('should evaluate core expressions', () => {
    const lits = new LitsFull({ modules: allBuiltinModules })
    expect(lits.run('1 + 2')).toBe(3)
  })

  it('should have all modules available via allBuiltinModules', () => {
    const lits = new LitsFull({ modules: allBuiltinModules })
    expect(lits.run('let { assert } = import("Assert"); assert(true)')).toBe(true)
    expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
    expect(lits.run('let g = import("Grid"); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
    expect(lits.run('let nt = import("Number-Theory"); nt.prime?(7)')).toBe(true)
  })

  it('should have reference data loaded (doc returns non-empty)', () => {
    const lits = new LitsFull({ modules: allBuiltinModules })
    const docString = lits.run('doc(+)') as string
    expect(docString.length).toBeGreaterThan(0)
    expect(docString).toContain('+')
  })

  it('should export apiReference', () => {
    expect(apiReference).toBeDefined()
    expect(Object.keys(apiReference).length).toBeGreaterThan(0)
  })

  it('should export allBuiltinModules with 7 modules', () => {
    expect(allBuiltinModules).toHaveLength(7)
  })
})

describe('individual module entry points', () => {
  it('assert module', () => {
    expect(assertModule.name).toBe('Assert')
    const lits = new Lits({ modules: [assertModule] })
    expect(lits.run('let { assert } = import("Assert"); assert(true)')).toBe(true)
    expect(() => lits.run('import("Vector")')).toThrow()
  })

  it('grid module', () => {
    expect(gridModule.name).toBe('Grid')
    const lits = new Lits({ modules: [gridModule] })
    expect(lits.run('let g = import("Grid"); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
  })

  it('random module', () => {
    expect(randomModule.name).toBe('Random')
    const lits = new Lits({ modules: [randomModule] })
    const result = lits.run('let r = import("Random"); r.random!()') as number
    expect(result).toBeGreaterThanOrEqual(0)
    expect(result).toBeLessThan(1)
  })

  it('vector module', () => {
    expect(vectorModule.name).toBe('Vector')
    const lits = new Lits({ modules: [vectorModule] })
    expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
  })

  it('linearAlgebra module', () => {
    expect(linearAlgebraModule.name).toBe('Linear-Algebra')
    const lits = new Lits({ modules: [linearAlgebraModule] })
    expect(lits.run('let la = import("Linear-Algebra"); la.dot([1, 2, 3], [4, 5, 6])')).toBe(32)
  })

  it('matrix module', () => {
    expect(matrixModule.name).toBe('Matrix')
    const lits = new Lits({ modules: [matrixModule] })
    expect(lits.run('let m = import("Matrix"); m.det([[1, 2], [3, 4]])')).toBe(-2)
  })

  it('numberTheory module', () => {
    expect(numberTheoryModule.name).toBe('Number-Theory')
    const lits = new Lits({ modules: [numberTheoryModule] })
    expect(lits.run('let nt = import("Number-Theory"); nt.prime?(7)')).toBe(true)
  })

  it('should allow combining multiple modules', () => {
    const lits = new Lits({ modules: [vectorModule, matrixModule] })
    expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
    expect(lits.run('let m = import("Matrix"); m.det([[1, 2], [3, 4]])')).toBe(-2)
    // Other modules should not be available
    expect(() => lits.run('import("Assert")')).toThrow()
  })
})
