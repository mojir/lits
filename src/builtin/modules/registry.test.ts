import { describe, expect, it } from 'vitest'
import { Lits } from '../../Lits/Lits'
import type { BuiltinNormalExpressions } from '../../builtin/interface'
import { allBuiltinModules } from '../../allModules'
import { assertModule } from './assert'
import { gridModule } from './grid'
import type { LitsModule } from './interface'
import { vectorModule } from './vector'

describe('module registration', () => {
  describe('default modules', () => {
    it('should have no modules by default', () => {
      const lits = new Lits()
      expect(() => lits.run('import(vector)')).toThrow('Unknown module')
      expect(() => lits.run('import(grid)')).toThrow('Unknown module')
      expect(() => lits.run('import(assert)')).toThrow('Unknown module')
    })
  })

  describe('all built-in modules', () => {
    it('should include all modules when allBuiltinModules is passed', () => {
      const lits = new Lits({ modules: allBuiltinModules })
      expect(lits.run('let v = import(vector); v.stdev([1, 2, 3])')).toBeCloseTo(0.8165, 3)
      expect(lits.run('let g = import(grid); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
      expect(lits.run('let { assert } = import(assert); assert(true)')).toBe(true)
    })
  })

  describe('custom modules', () => {
    it('should only include specified modules', () => {
      const lits = new Lits({ modules: [vectorModule] })
      expect(lits.run('let v = import(vector); v.stdev([1, 2, 3])')).toBeCloseTo(0.8165, 3)
      expect(() => lits.run('import(grid)')).toThrow('Unknown module')
      expect(() => lits.run('import(assert)')).toThrow('Unknown module')
    })

    it('should support empty modules list', () => {
      const lits = new Lits({ modules: [] })
      expect(() => lits.run('import(vector)')).toThrow('Unknown module')
      expect(() => lits.run('import(grid)')).toThrow('Unknown module')
    })

    it('should support multiple selected modules', () => {
      const lits = new Lits({ modules: [gridModule, assertModule] })
      expect(lits.run('let g = import(grid); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
      expect(lits.run('let { assert } = import(assert); assert(true)')).toBe(true)
      expect(() => lits.run('import(vector)')).toThrow('Unknown module')
    })
  })

  describe('user-defined module', () => {
    const temperatureFunctions: BuiltinNormalExpressions = {
      'c-to-f': {
        evaluate: ([celsius], sourceCodeInfo): number => {
          if (typeof celsius !== 'number') {
            throw new TypeError(`Expected a number${sourceCodeInfo ? ` at ${sourceCodeInfo}` : ''}`)
          }
          return celsius * 9 / 5 + 32
        },
        arity: { min: 1, max: 1 },
      },
      'f-to-c': {
        evaluate: ([fahrenheit], sourceCodeInfo): number => {
          if (typeof fahrenheit !== 'number') {
            throw new TypeError(`Expected a number${sourceCodeInfo ? ` at ${sourceCodeInfo}` : ''}`)
          }
          return (fahrenheit - 32) * 5 / 9
        },
        arity: { min: 1, max: 1 },
      },
    }

    const temperatureModule: LitsModule = {
      name: 'temperature',
      functions: temperatureFunctions,
    }

    it('should register and use a custom module', () => {
      const lits = new Lits({ modules: [temperatureModule] })
      expect(lits.run('let t = import(temperature); t.c-to-f(0)')).toBe(32)
      expect(lits.run('let t = import(temperature); t.c-to-f(100)')).toBe(212)
      expect(lits.run('let t = import(temperature); t.f-to-c(32)')).toBe(0)
      expect(lits.run('let t = import(temperature); t.f-to-c(212)')).toBe(100)
    })

    it('should work with destructuring import', () => {
      const lits = new Lits({ modules: [temperatureModule] })
      expect(lits.run('let { c-to-f, f-to-c } = import(temperature); c-to-f(f-to-c(72))')).toBe(72)
    })

    it('should work alongside built-in modules', () => {
      const lits = new Lits({ modules: [temperatureModule, vectorModule] })
      expect(lits.run('let t = import(temperature); t.c-to-f(0)')).toBe(32)
      expect(lits.run('let v = import(vector); v.stdev([1, 2, 3])')).toBeCloseTo(0.8165, 3)
    })

    it('should not be available when not registered', () => {
      const lits = new Lits({ modules: [vectorModule] })
      expect(() => lits.run('import(temperature)')).toThrow('Unknown module')
    })
  })
})
