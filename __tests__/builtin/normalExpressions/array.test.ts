import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import type { Arr } from '../../../src/interface'
import type { TestData } from '../../testUtils'
import { checkTestData, createTestData, getLitsVariants } from '../../testUtils'

let testData: TestData
const lits = getLitsVariants()

beforeEach(() => {
  testData = createTestData()
})

afterEach(() => {
  checkTestData()
})

describe('array functions', () => {
  describe('array', () => {
    it('samples', () => {
      expect(lits.run('[]')).toEqual([])
      expect(lits.run('(array 1)')).toEqual([1])
      expect(lits.run('(array 0 :1 nil true false (array []) (object))')).toEqual([0, '1', null, true, false, [[]], {}])
    })
    it('shorthand samples', () => {
      expect(lits.run('[]')).toEqual([])
      expect(lits.run('[1]')).toEqual([1])
      expect((lits.run('[nil]') as Arr)[0]).toEqual(null)
      expect(lits.run('[0 :1 nil true false [[]] (object)]')).toEqual([0, '1', null, true, false, [[]], {}])
    })
  })

  describe('range', () => {
    it('samples', () => {
      expect(lits.run('(range 0)')).toEqual([])
      expect(lits.run('(range 5)')).toEqual([0, 1, 2, 3, 4])
      expect(lits.run('(range -5)')).toEqual([0, -1, -2, -3, -4])
      expect(lits.run('(range 5 1)')).toEqual([5, 4, 3, 2])
      expect(lits.run('(range 1 5)')).toEqual([1, 2, 3, 4])
      expect(lits.run('(range 5 1 -2)')).toEqual([5, 3])
      expect(lits.run('(range 0 0.5 0.125)')).toEqual([0, 0.125, 0.25, 0.375])
      expect(() => lits.run('(range)')).toThrow()
      expect(() => lits.run('(range 0 2 1 1)')).toThrow()
      expect(() => lits.run('(range 0 2 0)')).toThrow()
      expect(() => lits.run('(range 0 0 0)')).toThrow()
      expect(() => lits.run('(range 1 "x")')).toThrow()
      expect(() => lits.run('(range false 1 2)')).toThrow()
      expect(() => lits.run('(range 0 2 "y")')).toThrow()
      expect(() => lits.run('(range (object) "x" "y")')).toThrow()
    })
  })

  describe('repeat', () => {
    it('samples', () => {
      expect(lits.run('(repeat 5 3)')).toEqual([5, 5, 5])
      expect(lits.run('(repeat :5 3)')).toEqual(['5', '5', '5'])
      expect(lits.run('(repeat :5 1)')).toEqual(['5'])
      expect(lits.run('(repeat :5 0)')).toEqual([])
      expect(() => lits.run('(repeat :5 1.3)')).toThrow()
      expect(() => lits.run('(repeat :5 -10)')).toThrow()
      expect(() => lits.run('(repeat 10)')).toThrow()
      expect(() => lits.run('(repeat :5)')).toThrow()
      expect(() => lits.run('(repeat)')).toThrow()
    })
  })

  describe('flatten', () => {
    it('samples', () => {
      expect(lits.run('(flatten [1 2 [3 4] 5])')).toEqual([1, 2, 3, 4, 5])
      expect(lits.run('(flatten [1 2 [3 [4 [5]]] 6])')).toEqual([1, 2, 3, 4, 5, 6])
      expect(lits.run('(flatten {})')).toEqual([])
      expect(lits.run('(flatten 12)')).toEqual([])
      expect(lits.run('(flatten true)')).toEqual([])
      expect(lits.run('(flatten false)')).toEqual([])
      expect(lits.run('(flatten nil)')).toEqual([])
      expect(lits.run('(flatten #"abc")')).toEqual([])
      expect(() => lits.run('(flatten [] [])')).toThrow()
      expect(() => lits.run('(flatten)')).toThrow()
    })
    it('immutability', () => {
      lits.run('(flatten nestedArray)', { values: testData })
    })
  })

  describe('mapcat', () => {
    it('samples', () => {
      expect(lits.run('(mapcat [[3 2 1 0] [6 5 4] [9 8 7]] reverse)')).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
      expect(lits.run('(mapcat [[3 2 1 0] [6 [5] 4] [9 8 7]], reverse)')).toEqual([0, 1, 2, 3, 4, [5], 6, 7, 8, 9])
      expect(lits.run('(defn foo [n] [(- n 1) n (+ n 1)]) (mapcat [1 2 3] foo)')).toEqual([0, 1, 2, 1, 2, 3, 2, 3, 4])
      expect(lits.run('(mapcat [[1 2] [2 2] [2 3]] #(remove even? %1))')).toEqual([1, 3])
    })
  })
})
