import { describe, expect, it } from 'vitest'
import { Lits } from '../../Lits/Lits'
import { allBuiltinNamespaces } from '../../allNamespaces'
import { assertNamespace } from './assert'
import { gridNamespace } from './grid'
import { vectorNamespace } from './vector'

describe('namespace registration', () => {
  describe('default namespaces', () => {
    it('should have no namespaces by default', () => {
      const lits = new Lits()
      expect(() => lits.run('import("Vector")')).toThrow('Unknown namespace')
      expect(() => lits.run('import("Grid")')).toThrow('Unknown namespace')
      expect(() => lits.run('import("Assert")')).toThrow('Unknown namespace')
    })
  })

  describe('all built-in namespaces', () => {
    it('should include all namespaces when allBuiltinNamespaces is passed', () => {
      const lits = new Lits({ namespaces: allBuiltinNamespaces })
      expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
      expect(lits.run('let g = import("Grid"); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
      expect(lits.run('let { assert } = import("Assert"); assert(true)')).toBe(true)
    })
  })

  describe('custom namespaces', () => {
    it('should only include specified namespaces', () => {
      const lits = new Lits({ namespaces: [vectorNamespace] })
      expect(lits.run('let v = import("Vector"); v.mean([1, 2, 3])')).toBe(2)
      expect(() => lits.run('import("Grid")')).toThrow('Unknown namespace')
      expect(() => lits.run('import("Assert")')).toThrow('Unknown namespace')
    })

    it('should support empty namespaces list', () => {
      const lits = new Lits({ namespaces: [] })
      expect(() => lits.run('import("Vector")')).toThrow('Unknown namespace')
      expect(() => lits.run('import("Grid")')).toThrow('Unknown namespace')
    })

    it('should support multiple selected namespaces', () => {
      const lits = new Lits({ namespaces: [gridNamespace, assertNamespace] })
      expect(lits.run('let g = import("Grid"); g.row([[1, 2], [3, 4]], 0)')).toEqual([1, 2])
      expect(lits.run('let { assert } = import("Assert"); assert(true)')).toBe(true)
      expect(() => lits.run('import("Vector")')).toThrow('Unknown namespace')
    })
  })
})
