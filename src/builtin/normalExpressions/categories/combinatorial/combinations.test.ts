import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'

const lits = new Lits()
describe('combinations', () => {
  describe('c:combinations', () => {
    it('should return the combinations of n elements from a set', () => {
      expect(lits.run('c:combinations(["a", "b", "c"], 0)')).toEqual([[]])
      expect(lits.run('c:combinations(["a", "b", "c"], 2)')).toEqual([
        ['a', 'b'],
        ['a', 'c'],
        ['b', 'c'],
      ])
      expect(lits.run('c:combinations(["a", "b", "c"], 3)')).toEqual([
        ['a', 'b', 'c'],
      ])
      expect(lits.run('c:combinations(["a", "b", "c"], 1)')).toEqual([
        ['a'],
        ['b'],
        ['c'],
      ])
      expect(lits.run('c:combinations(["a", "b", "c"], 0)')).toEqual([
        [],
      ])
    })
  })
  describe('c:count-combinations', () => {
    it('should return the number of combinations from n, k', () => {
      expect(lits.run('c:count-combinations(2, 2)')).toEqual(1)
      expect(lits.run('c:count-combinations(3, 2)')).toEqual(3)
      expect(lits.run('c:count-combinations(4, 2)')).toEqual(6)
      expect(lits.run('c:count-combinations(5, 3)')).toEqual(10)
      expect(lits.run('c:count-combinations(6, 4)')).toEqual(15)
      expect(lits.run('c:count-combinations(7, 5)')).toEqual(21)
    })
  })
})
