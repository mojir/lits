import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'
import { LitsError } from '../../../../../errors'

const lits = new Lits()
describe('partitions', () => {
  describe('nth:partitions', () => {
    it('should return the partitions of a number', () => {
      expect(lits.run('nth:partitions(0)')).toEqual([[]])
      expect(lits.run('nth:partitions(1)')).toEqual([[1]])
      expect(lits.run('nth:partitions(4)')).toEqual([
        [4],
        [3, 1],
        [2, 2],
        [2, 1, 1],
        [1, 1, 1, 1],
      ])
      expect(lits.run('nth:partitions(5)')).toEqual([
        [5],
        [4, 1],
        [3, 2],
        [3, 1, 1],
        [2, 2, 1],
        [2, 1, 1, 1],
        [1, 1, 1, 1, 1],
      ])
      expect(lits.run('nth:partitions(0)')).toEqual([[]])
    })
  })
  describe('nth:count-partitions', () => {
    it('should return the number of partitions from n', () => {
      expect(lits.run('nth:count-partitions(0)')).toEqual(1)
      expect(lits.run('nth:count-partitions(1)')).toEqual(1)
      expect(lits.run('nth:count-partitions(2)')).toEqual(2)
      expect(lits.run('nth:count-partitions(3)')).toEqual(3)
      expect(lits.run('nth:count-partitions(4)')).toEqual(5)
      expect(lits.run('nth:count-partitions(5)')).toEqual(7)
      expect(lits.run('nth:count-partitions(6)')).toEqual(11)
      expect(() => lits.run('nth:count-partitions(300)')).toThrow(LitsError)
    })
  })
})
