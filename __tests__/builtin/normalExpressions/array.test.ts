import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import type { TestData } from '../../testUtils'
import { checkTestData, createTestData } from '../../testUtils'
import { Lits } from '../../../src/Lits/Lits'
import { vectorModule } from '../../../src/builtin/modules/vector'
import { LitsError } from '../../../src/errors'

let testData: TestData
const lits = new Lits({ modules: [vectorModule] })

beforeEach(() => {
  testData = createTestData()
})

afterEach(() => {
  checkTestData()
})

describe('array functions', () => {
  describe('range', () => {
    it('samples', () => {
      expect(lits.run('range(0)')).toEqual([])
      expect(lits.run('range(5)')).toEqual([0, 1, 2, 3, 4])
      expect(lits.run('range(-5)')).toEqual([0, -1, -2, -3, -4])
      expect(lits.run('range(5, 1)')).toEqual([5, 4, 3, 2])
      expect(lits.run('range(1, 5)')).toEqual([1, 2, 3, 4])
      expect(lits.run('1 range 5')).toEqual([1, 2, 3, 4])
      expect(lits.run('range(5, 1, -2)')).toEqual([5, 3])
      expect(lits.run('range(0, 0.5, 0.125)')).toEqual([0, 0.125, 0.25, 0.375])
      expect(() => lits.run('range()')).toThrow(LitsError)
      expect(() => lits.run('range(0, 2, 1, 1)')).toThrow(LitsError)
      expect(() => lits.run('range(0, 2, 0)')).toThrow(LitsError)
      expect(() => lits.run('range(0, 0, 0)')).toThrow(LitsError)
      expect(() => lits.run('range(1, "x")')).toThrow(LitsError)
      expect(() => lits.run('range(false, 1, 2)')).toThrow(LitsError)
      expect(() => lits.run('range(0, 2, "y")')).toThrow(LitsError)
      expect(() => lits.run('range({}, "x", "y")')).toThrow(LitsError)
    })
  })

  describe('repeat', () => {
    it('samples', () => {
      expect(lits.run('repeat(5, 3)')).toEqual([5, 5, 5])
      expect(lits.run('repeat("5", 3)')).toEqual(['5', '5', '5'])
      expect(lits.run('"5" repeat 3')).toEqual(['5', '5', '5'])
      expect(lits.run('repeat("5", 1)')).toEqual(['5'])
      expect(lits.run('repeat("5", 0)')).toEqual([])
      expect(() => lits.run('repeat("5", 1.3)')).toThrow(LitsError)
      expect(() => lits.run('repeat("5", -10)')).toThrow(LitsError)
      expect(() => lits.run('repeat(10)')).toThrow(LitsError)
      expect(() => lits.run('repeat("5")')).toThrow(LitsError)
      expect(() => lits.run('repeat()')).toThrow(LitsError)
    })
  })

  describe('flatten', () => {
    it('samples', () => {
      expect(lits.run('flatten([1, 2, [3, 4], 5])')).toEqual([1, 2, 3, 4, 5])
      expect(lits.run('flatten([1, 2, [3, [4, [5]]], 6])')).toEqual([1, 2, 3, 4, 5, 6])
      expect(lits.run('flatten([1, 2, [3, [4, [5]]], 6], 1)')).toEqual([1, 2, 3, [4, [5]], 6])
      expect(() => lits.run('flatten({})')).toThrow(LitsError)
      expect(() => lits.run('flatten(12)')).toThrow(LitsError)
      expect(() => lits.run('flatten(true)')).toThrow(LitsError)
      expect(() => lits.run('flatten(false)')).toThrow(LitsError)
      expect(() => lits.run('flatten(null)')).toThrow(LitsError)
      expect(() => lits.run('flatten(#"abc")')).toThrow(LitsError)
      expect(() => lits.run('flatten([], [])')).toThrow(LitsError)
      expect(() => lits.run('flatten()')).toThrow(LitsError)
    })
    it('immutability', () => {
      lits.run('flatten(nestedArray)', { values: testData })
    })
  })

  describe('mapcat', () => {
    it('samples', () => {
      expect(lits.run('mapcat([[3, 2, 1, 0], [6, 5, 4], [9, 8, 7]], reverse)')).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
      expect(lits.run('mapcat([[3, 2, 1, 0], [6, [5], 4], [9, 8, 7]], reverse)')).toEqual([0, 1, 2, 3, 4, [5], 6, 7, 8, 9])
      expect(lits.run('let foo = (n) -> do [-(n, 1), n, +(n, 1)] end; mapcat([1, 2, 3], foo)')).toEqual([0, 1, 2, 1, 2, 3, 2, 3, 4])
      expect(lits.run('mapcat([[1, 2], [2, 2], [2, 3]], -> $ filter odd?)')).toEqual([1, 3])
    })
  })

  describe('running-fn', () => {
    it('samples', () => {
      expect(lits.run('let v = import("Vector"); running-fn([1, 2, 3], v.sum)')).toEqual([1, 3, 6])
      expect(() => lits.run('running-fn(1)')).toThrow(LitsError)
      expect(() => lits.run('let v = import("Vector"); running-fn(1, v.sum)')).toThrow(LitsError)
      expect(() => lits.run('let v = import("Vector"); running-fn(1, v.sum, null)')).toThrow(LitsError)
    })
  })
  describe('moving-fn', () => {
    it('samples', () => {
      expect(lits.run('let v = import("Vector"); moving-fn([1, 2, 3], 2, v.sum)')).toEqual([3, 5])
      expect(lits.run('let v = import("Vector"); moving-fn([1, 2, 3], 1, v.sum)')).toEqual([1, 2, 3])
      expect(lits.run('let v = import("Vector"); moving-fn([1, 2, 3], 3, v.sum)')).toEqual([6])
      expect(() => lits.run('let v = import("Vector"); moving-fn([1, 2, 3], 4, v.sum)')).toThrow(LitsError)
      expect(() => lits.run('moving-fn(1)')).toThrow(LitsError)
      expect(() => lits.run('moving-fn(1, 2)')).toThrow(LitsError)
      expect(() => lits.run('moving-fn(1, 2, null)')).toThrow(LitsError)
    })
  })
})
