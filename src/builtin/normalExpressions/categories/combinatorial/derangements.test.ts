import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'

const lits = new Lits()
describe('derangements', () => {
  describe('c:derangements', () => {
    it('should return all derangements of a set', () => {
      expect(lits.run('c:derangements([1, 2, 3])')).toEqual([
        [2, 3, 1],
        [3, 1, 2],
      ])
      expect(lits.run('c:derangements(["a", "b", "c", "d"])')).toEqual([
        ['b', 'a', 'd', 'c'],
        ['b', 'c', 'd', 'a'],
        ['b', 'd', 'a', 'c'],
        ['c', 'a', 'd', 'b'],
        ['c', 'd', 'a', 'b'],
        ['c', 'd', 'b', 'a'],
        ['d', 'a', 'b', 'c'],
        ['d', 'c', 'a', 'b'],
        ['d', 'c', 'b', 'a'],
      ])
    })
  })
  describe('c:count-derangements', () => {
    it('should return the number of derangements from n', () => {
      expect(lits.run('c:count-derangements(1)')).toEqual(0)
      expect(lits.run('c:count-derangements(2)')).toEqual(1)
      expect(lits.run('c:count-derangements(3)')).toEqual(2)
      expect(lits.run('c:count-derangements(4)')).toEqual(9)
      expect(lits.run('c:count-derangements(5)')).toEqual(44)
      expect(lits.run('c:count-derangements(6)')).toEqual(265)
    })
  })
})
