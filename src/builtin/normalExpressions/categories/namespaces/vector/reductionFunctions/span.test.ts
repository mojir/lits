import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()

describe('vec:span', () => {
  it('should calculate the span of a vector', () => {
    expect(lits.run('vec:span([1, 2, 3, 4])')).toEqual(3)
    expect(lits.run('vec:span([1, 2, 3, 4, 5])')).toEqual(4)
    expect(lits.run('vec:span([1])')).toEqual(0)
    expect(lits.run('vec:span([])')).toEqual(0)
  })
  it('should calculate the moving span of a vector', () => {
    expect(lits.run('vec:moving-span([1, 2, 4, 7, 11, 16], 4)')).toEqual([6, 9, 12])
    expect(lits.run('vec:moving-span([1, 2, 4, 7, 11, 16], 5)')).toEqual([10, 14])
    expect(lits.run('vec:moving-span([1, 2, 4, 7, 11, 16], 6)')).toEqual([15])
    expect(lits.run('vec:moving-span([], 0)')).toEqual([])
  })
  it('should calculate the centered moving span of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-span([1, 2, 4, 7, 11], 4)')).toEqual([null, null, 6, 9, null])
    expect(lits.run('vec:centered-moving-span([], 0, 0, 100)')).toEqual([])
  })
  it('should calculate the running span of a vector', () => {
    expect(lits.run('vec:running-span([1, 2, 3, 4, 5, 6])')).toEqual([0, 1, 2, 3, 4, 5])
    expect(lits.run('vec:running-span([-1, -2, -3])')).toEqual([0, 1, 2])
    expect(lits.run('vec:running-span([])')).toEqual([])
  })
})
