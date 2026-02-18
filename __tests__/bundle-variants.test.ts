/**
 * Bundle variant tests — verify that entry points work correctly.
 *
 * - Minimal (src/index.ts): Lits core, no namespace definitions, no docs data
 * - Full (src/full.ts): Lits core + all namespaces + docs/reference data
 * - Individual namespace entry points: export the correct namespace
 */
import { describe, expect, it } from 'vitest'
import { Lits } from '../src/index'
import { Lits as LitsFull, allBuiltinNamespaces, apiReference } from '../src/full'
import { assertNamespace } from '../src/namespaces/assert'
import { gridNamespace } from '../src/namespaces/grid'
import { randomNamespace } from '../src/namespaces/random'
import { vectorNamespace } from '../src/namespaces/vector'
import { linearAlgebraNamespace } from '../src/namespaces/linearAlgebra'
import { matrixNamespace } from '../src/namespaces/matrix'
import { numberTheoryNamespace } from '../src/namespaces/numberTheory'

describe('minimal entry point (src/index.ts)', () => {
  it('should evaluate core expressions without namespaces', () => {
    const lits = new Lits()
    expect(lits.run('1 + 2')).toBe(3)
    expect(lits.run('map([1, 2, 3], inc)')).toEqual([2, 3, 4])
    expect(lits.run('let x = 10; x * x')).toBe(100)
  })

  it('should default to no namespaces', () => {
    const lits = new Lits()
    expect(() => lits.run('import("Assert")')).toThrow()
  })

  it('should accept individual namespaces passed in', () => {
    const lits = new Lits({ namespaces: [vectorNamespace] })
    expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
  })

  // NOTE: cannot test "doc returns empty" here because importing src/full.ts
  // triggers initReferenceData as a side effect. That behavior is tested
  // indirectly: without the import, doc(+) would return ''.
})

describe('full entry point (src/full.ts)', () => {
  it('should evaluate core expressions', () => {
    const lits = new LitsFull({ namespaces: allBuiltinNamespaces })
    expect(lits.run('1 + 2')).toBe(3)
  })

  it('should have all namespaces available via allBuiltinNamespaces', () => {
    const lits = new LitsFull({ namespaces: allBuiltinNamespaces })
    expect(lits.run('let { assert } = import("Assert"); assert(true)')).toBe(true)
    expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
    expect(lits.run('let g = import("Grid"); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
    expect(lits.run('let nt = import("Number-Theory"); nt.prime?(7)')).toBe(true)
  })

  it('should have reference data loaded (doc returns non-empty)', () => {
    const lits = new LitsFull({ namespaces: allBuiltinNamespaces })
    const docString = lits.run('doc(+)') as string
    expect(docString.length).toBeGreaterThan(0)
    expect(docString).toContain('+')
  })

  it('should export apiReference', () => {
    expect(apiReference).toBeDefined()
    expect(Object.keys(apiReference).length).toBeGreaterThan(0)
  })

  it('should export allBuiltinNamespaces with 7 namespaces', () => {
    expect(allBuiltinNamespaces).toHaveLength(7)
  })
})

describe('individual namespace entry points', () => {
  it('assert namespace', () => {
    expect(assertNamespace.name).toBe('Assert')
    const lits = new Lits({ namespaces: [assertNamespace] })
    expect(lits.run('let { assert } = import("Assert"); assert(true)')).toBe(true)
    expect(() => lits.run('import("Vector")')).toThrow()
  })

  it('grid namespace', () => {
    expect(gridNamespace.name).toBe('Grid')
    const lits = new Lits({ namespaces: [gridNamespace] })
    expect(lits.run('let g = import("Grid"); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
  })

  it('random namespace', () => {
    expect(randomNamespace.name).toBe('Random')
    const lits = new Lits({ namespaces: [randomNamespace] })
    const result = lits.run('let r = import("Random"); r.random!()') as number
    expect(result).toBeGreaterThanOrEqual(0)
    expect(result).toBeLessThan(1)
  })

  it('vector namespace', () => {
    expect(vectorNamespace.name).toBe('Vector')
    const lits = new Lits({ namespaces: [vectorNamespace] })
    expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
  })

  it('linearAlgebra namespace', () => {
    expect(linearAlgebraNamespace.name).toBe('Linear-Algebra')
    const lits = new Lits({ namespaces: [linearAlgebraNamespace] })
    expect(lits.run('let la = import("Linear-Algebra"); la.dot([1, 2, 3], [4, 5, 6])')).toBe(32)
  })

  it('matrix namespace', () => {
    expect(matrixNamespace.name).toBe('Matrix')
    const lits = new Lits({ namespaces: [matrixNamespace] })
    expect(lits.run('let m = import("Matrix"); m.det([[1, 2], [3, 4]])')).toBe(-2)
  })

  it('numberTheory namespace', () => {
    expect(numberTheoryNamespace.name).toBe('Number-Theory')
    const lits = new Lits({ namespaces: [numberTheoryNamespace] })
    expect(lits.run('let nt = import("Number-Theory"); nt.prime?(7)')).toBe(true)
  })

  it('should allow combining multiple namespaces', () => {
    const lits = new Lits({ namespaces: [vectorNamespace, matrixNamespace] })
    expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
    expect(lits.run('let m = import("Matrix"); m.det([[1, 2], [3, 4]])')).toBe(-2)
    // Other namespaces should not be available
    expect(() => lits.run('import("Assert")')).toThrow()
  })
})
