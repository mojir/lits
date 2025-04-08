import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('permutations', () => {
  describe('n:permutations', () => {
    it('should return the permutations of a set', () => {
      expect(lits.run('n:permutations(["a", "b", "c"])')).toEqual([
        ['a', 'b', 'c'],
        ['a', 'c', 'b'],
        ['b', 'a', 'c'],
        ['b', 'c', 'a'],
        ['c', 'a', 'b'],
        ['c', 'b', 'a'],
      ])
      expect(lits.run('n:permutations(["a", "b"])')).toEqual([
        ['a', 'b'],
        ['b', 'a'],
      ])
      expect(lits.run('n:permutations(["a"])')).toEqual([
        ['a'],
      ])
      expect(lits.run('n:permutations([])')).toEqual([
        [],
      ])
    })
  })
  describe('n:count-permutations', () => {
    it('should return the number of permutations from n, k', () => {
      expect(lits.run('n:count-permutations(2, 2)')).toEqual(2)
      expect(lits.run('n:count-permutations(3, 2)')).toEqual(6)
      expect(lits.run('n:count-permutations(4, 2)')).toEqual(12)
      expect(lits.run('n:count-permutations(5, 3)')).toEqual(60)
      expect(lits.run('n:count-permutations(6, 4)')).toEqual(360)
      expect(lits.run('n:count-permutations(7, 5)')).toEqual(2520)
    })
  })
})
