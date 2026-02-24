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
import { linearAlgebraModule } from '../src/modules/linear-algebra'
import { matrixModule } from '../src/modules/matrix'
import { numberTheoryModule } from '../src/modules/number-theory'
import { stringUtilsModule } from '../src/modules/string'
import { collectionUtilsModule } from '../src/modules/collection'
import { sequenceUtilsModule } from '../src/modules/sequence'
import { bitwiseUtilsModule } from '../src/modules/bitwise'
import { functionalUtilsModule } from '../src/modules/functional'
import { mathUtilsModule } from '../src/modules/math'
import { convertModule } from '../src/modules/convert'

describe('minimal entry point (src/index.ts)', () => {
  it('should evaluate core expressions without modules', () => {
    const lits = new Lits()
    expect(lits.run('1 + 2')).toBe(3)
    expect(lits.run('map([1, 2, 3], inc)')).toEqual([2, 3, 4])
    expect(lits.run('let x = 10; x * x')).toBe(100)
  })

  it('should default to no modules', () => {
    const lits = new Lits()
    expect(() => lits.run('import(assert)')).toThrow()
  })

  it('should accept individual modules passed in', () => {
    const lits = new Lits({ modules: [vectorModule] })
    expect(lits.run('let v = import(vector); v.stdev([1, 2, 3])')).toBeCloseTo(0.8165, 3)
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
    expect(lits.run('let { assert } = import(assert); assert(true)')).toBe(true)
    expect(lits.run('let v = import(vector); v.stdev([1, 2, 3])')).toBeCloseTo(0.8165, 3)
    expect(lits.run('let g = import(grid); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
    expect(lits.run('let nt = import(number-theory); nt.prime?(7)')).toBe(true)
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

  it('should export allBuiltinModules with 14 modules', () => {
    expect(allBuiltinModules).toHaveLength(14)
  })
})

describe('individual module entry points', () => {
  it('assert module', () => {
    expect(assertModule.name).toBe('assert')
    const lits = new Lits({ modules: [assertModule] })
    expect(lits.run('let { assert } = import(assert); assert(true)')).toBe(true)
    expect(() => lits.run('import(vector)')).toThrow()
  })

  it('grid module', () => {
    expect(gridModule.name).toBe('grid')
    const lits = new Lits({ modules: [gridModule] })
    expect(lits.run('let g = import(grid); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
  })

  it('random module', () => {
    expect(randomModule.name).toBe('random')
    const lits = new Lits({ modules: [randomModule] })
    const result = lits.run('let r = import(random); r.random!()') as number
    expect(result).toBeGreaterThanOrEqual(0)
    expect(result).toBeLessThan(1)
  })

  it('vector module', () => {
    expect(vectorModule.name).toBe('vector')
    const lits = new Lits({ modules: [vectorModule] })
    expect(lits.run('let v = import(vector); v.stdev([1, 2, 3])')).toBeCloseTo(0.8165, 3)
  })

  it('linearAlgebra module', () => {
    expect(linearAlgebraModule.name).toBe('linear-algebra')
    const lits = new Lits({ modules: [linearAlgebraModule] })
    expect(lits.run('let la = import(linear-algebra); la.dot([1, 2, 3], [4, 5, 6])')).toBe(32)
  })

  it('matrix module', () => {
    expect(matrixModule.name).toBe('matrix')
    const lits = new Lits({ modules: [matrixModule] })
    expect(lits.run('let m = import(matrix); m.det([[1, 2], [3, 4]])')).toBe(-2)
  })

  it('numberTheory module', () => {
    expect(numberTheoryModule.name).toBe('number-theory')
    const lits = new Lits({ modules: [numberTheoryModule] })
    expect(lits.run('let nt = import(number-theory); nt.prime?(7)')).toBe(true)
  })

  it('stringUtils module', () => {
    expect(stringUtilsModule.name).toBe('string')
    const lits = new Lits({ modules: [stringUtilsModule] })
    expect(lits.run('let { capitalize } = import(string); capitalize("albert")')).toBe('Albert')
  })

  it('collectionUtils module', () => {
    expect(collectionUtilsModule.name).toBe('collection')
    const lits = new Lits({ modules: [collectionUtilsModule] })
    expect(lits.run('let cu = import(collection); cu.every?([1, 2, 3], number?)')).toBe(true)
  })

  it('sequenceUtils module', () => {
    expect(sequenceUtilsModule.name).toBe('sequence')
    const lits = new Lits({ modules: [sequenceUtilsModule] })
    expect(lits.run('let su = import(sequence); su.distinct([1, 2, 3, 1, 3, 5])')).toEqual([1, 2, 3, 5])
  })

  it('bitwiseUtils module', () => {
    expect(bitwiseUtilsModule.name).toBe('bitwise')
    const lits = new Lits({ modules: [bitwiseUtilsModule] })
    expect(lits.run('let b = import(bitwise); b.bit-not(0)')).toBe(-1)
  })

  it('functionalUtils module', () => {
    expect(functionalUtilsModule.name).toBe('functional')
    const lits = new Lits({ modules: [functionalUtilsModule] })
    expect(lits.run('let f = import(functional); (f.complement(zero?))(1)')).toBe(true)
  })

  it('mathUtils module', () => {
    expect(mathUtilsModule.name).toBe('math')
    const lits = new Lits({ modules: [mathUtilsModule] })
    expect(lits.run('let m = import(math); m.sin(0)')).toBe(0)
    expect(() => lits.run('let m = import(math); m.sin("hello")')).toThrow()
  })

  it('convert module', () => {
    expect(convertModule.name).toBe('convert')
    const lits = new Lits({ modules: [convertModule] })
    expect(lits.run('let c = import(convert); c.c->f(100)')).toBe(212)
    expect(lits.run('let c = import(convert); c.kg->lb(1)')).toBeCloseTo(2.20462, 4)
  })

  it('should allow combining multiple modules', () => {
    const lits = new Lits({ modules: [vectorModule, matrixModule] })
    expect(lits.run('let v = import(vector); v.stdev([1, 2, 3])')).toBeCloseTo(0.8165, 3)
    expect(lits.run('let m = import(matrix); m.det([[1, 2], [3, 4]])')).toBe(-2)
    // Other modules should not be available
    expect(() => lits.run('import(assert)')).toThrow()
  })
})
