import { describe, expect, it } from 'vitest'
import { Lits } from '../../Lits/Lits'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("TEMP-nth"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('permutations', () => {
  describe('nth:permutations', () => {
    it('should return the permutations of a set', () => {
      expect(runNth('nth:permutations(["a", "b", "c"])')).toEqual([
        ['a', 'b', 'c'],
        ['a', 'c', 'b'],
        ['b', 'a', 'c'],
        ['b', 'c', 'a'],
        ['c', 'a', 'b'],
        ['c', 'b', 'a'],
      ])
      expect(runNth('nth:permutations(["a", "b"])')).toEqual([
        ['a', 'b'],
        ['b', 'a'],
      ])
      expect(runNth('nth:permutations(["a"])')).toEqual([
        ['a'],
      ])
      expect(runNth('nth:permutations([])')).toEqual([
        [],
      ])
    })
  })
  describe('nth:count-permutations', () => {
    it('should return the number of permutations from n, k', () => {
      expect(runNth('nth:count-permutations(2, 2)')).toEqual(2)
      expect(runNth('nth:count-permutations(3, 2)')).toEqual(6)
      expect(runNth('nth:count-permutations(4, 2)')).toEqual(12)
      expect(runNth('nth:count-permutations(5, 3)')).toEqual(60)
      expect(runNth('nth:count-permutations(6, 4)')).toEqual(360)
      expect(runNth('nth:count-permutations(7, 5)')).toEqual(2520)
    })
  })
})
