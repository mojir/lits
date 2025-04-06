import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'

const lits = new Lits()
describe('partitions', () => {
  describe('c:partitions', () => {
    it('should return the partitions of a number', () => {
      expect(lits.run('c:partitions(0)')).toEqual([[]])
      expect(lits.run('c:partitions(1)')).toEqual([[1]])
      expect(lits.run('c:partitions(4)')).toEqual([
        [4],
        [3, 1],
        [2, 2],
        [2, 1, 1],
        [1, 1, 1, 1],
      ])
      expect(lits.run('c:partitions(5)')).toEqual([
        [5],
        [4, 1],
        [3, 2],
        [3, 1, 1],
        [2, 2, 1],
        [2, 1, 1, 1],
        [1, 1, 1, 1, 1],
      ])
      expect(lits.run('c:partitions(0)')).toEqual([[]])
    })
  })
  describe('c:count-partitions', () => {
    it('should return the number of partitions from n', () => {
      expect(lits.run('c:count-partitions(0)')).toEqual(1)
      expect(lits.run('c:count-partitions(1)')).toEqual(1)
      expect(lits.run('c:count-partitions(2)')).toEqual(2)
      expect(lits.run('c:count-partitions(3)')).toEqual(3)
      expect(lits.run('c:count-partitions(4)')).toEqual(5)
      expect(lits.run('c:count-partitions(5)')).toEqual(7)
      expect(lits.run('c:count-partitions(6)')).toEqual(11)
      expect(() => lits.run('c:count-partitions(300)')).toThrow()
    })
  })
})
