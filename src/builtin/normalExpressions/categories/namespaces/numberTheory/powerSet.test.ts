import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('powerSet', () => {
  describe('nth:power-set', () => {
    it('should return the power set of a set', () => {
      expect(lits.run('nth:power-set([1, 2, 3])')).toEqual([
        [],
        [1],
        [2],
        [1, 2],
        [3],
        [1, 3],
        [2, 3],
        [1, 2, 3],
      ])
    })
  })
  describe('nth:count-power-set', () => {
    it('should return the size of a power set from a set with length n', () => {
      expect(lits.run('nth:count-power-set(0)')).toEqual(1)
      expect(lits.run('nth:count-power-set(1)')).toEqual(2)
      expect(lits.run('nth:count-power-set(2)')).toEqual(4)
      expect(lits.run('nth:count-power-set(3)')).toEqual(8)
      expect(lits.run('nth:count-power-set(4)')).toEqual(16)
      expect(lits.run('nth:count-power-set(5)')).toEqual(32)
      expect(lits.run('nth:count-power-set(54)')).toBe(Number.POSITIVE_INFINITY)
    })
  })
})
