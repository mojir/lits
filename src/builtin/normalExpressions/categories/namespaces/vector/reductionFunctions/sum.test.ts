import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()

describe('vec:sum', () => {
  it('should calculate sum of a vector', () => {
    expect(lits.run('vec:sum([1, 2, 3])')).toEqual(6)
    expect(lits.run('vec:sum([1, -2, 3])')).toEqual(2)
    expect(lits.run('vec:sum([-1, -2, -3])')).toEqual(-6)
    expect(lits.run('vec:sum([0])')).toEqual(0)
    expect(lits.run('vec:sum([])')).toEqual(0)
  })
  it('should calculate the moving sum of a vector', () => {
    expect(lits.run('vec:moving-sum([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:moving-sum([1, 2, 3, 4, 5, 6], 3)')).toEqual([6, 9, 12, 15])
    expect(lits.run('vec:moving-sum([1, 2, 3, 4, 5, 6], 6)')).toEqual([21])
    expect(lits.run('vec:moving-sum([1, -2, -3], 2)')).toEqual([-1, -5])
    expect(lits.run('vec:moving-sum([], 0)')).toEqual([])
  })
  it('should calculate the centered moving sum of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-sum([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:centered-moving-sum([1, 2, 3, 4, 5, 6], 2)')).toEqual([null, 3, 5, 7, 9, 11])
    expect(lits.run('vec:centered-moving-sum([1, 2, 3, 4, 5, 6], 2, 10)')).toEqual([11, 3, 5, 7, 9, 11])
    expect(lits.run('vec:centered-moving-sum([1, 2, 3, 4, 5, 6], 3)')).toEqual([null, 6, 9, 12, 15, null])
    expect(lits.run('vec:centered-moving-sum([1, 2, 3, 4, 5, 6], 6)')).toEqual([null, null, null, 21, null, null])
    expect(lits.run('vec:centered-moving-sum([], 0)')).toEqual([])
  })
  it('should calculate the running sum of a vector', () => {
    expect(lits.run('vec:running-sum([1, 2, 3, 4, 5, 6])')).toEqual([1, 3, 6, 10, 15, 21])
    expect(lits.run('vec:running-sum([1, -2, -3])')).toEqual([1, -1, -4])
    expect(lits.run('vec:running-sum([-1, -2, -3])')).toEqual([-1, -3, -6])
    expect(lits.run('vec:running-sum([0])')).toEqual([0])
    expect(lits.run('vec:running-sum([])')).toEqual([])
  })
})
