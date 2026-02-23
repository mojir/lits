import { describe, expect, it } from 'vitest'
import { Lits } from '../../Lits/Lits'
import { allBuiltinModules } from '../../allModules'
import { assertModule } from './assert'
import { gridModule } from './grid'
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
      expect(lits.run('let v = import(vector); v.mean([1, 2, 3])')).toBe(2)
      expect(lits.run('let g = import(grid); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
      expect(lits.run('let { assert } = import(assert); assert(true)')).toBe(true)
    })
  })

  describe('custom modules', () => {
    it('should only include specified modules', () => {
      const lits = new Lits({ modules: [vectorModule] })
      expect(lits.run('let v = import(vector); v.mean([1, 2, 3])')).toBe(2)
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
})
