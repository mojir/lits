import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('vec:median', () => {
  it('should calculate the median of a vector', () => {
    expect(lits.run('vec:median([1, 2, 3])')).toEqual(2)
    expect(lits.run('vec:median([1, 2, 3, 4, 5, 6])')).toEqual(3.5)
    expect(lits.run('vec:median([1, -3, 2])')).toEqual(1)
    expect(lits.run('vec:median([-1, -2, -3])')).toEqual(-2)
    expect(lits.run('vec:median([0])')).toEqual(0)
    expect(() => lits.run('vec:median([])')).toThrowError(LitsError)
  })
  it('should calculate the moving median of a vector', () => {
    expect(lits.run('vec:moving-median([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:moving-median([1, 2, 3, 4, 5, 6], 3)')).toEqual([2, 3, 4, 5])
    expect(lits.run('vec:moving-median([1, 2, 3, 4, 5, 6], 6)')).toEqual([3.5])
    expect(lits.run('vec:moving-median([1, 2, 3, 4, 5, 6], 100)')).toEqual([3.5])
  })
  it('should calculate the centered moving median of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-median([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:centered-moving-median([1, 2, 3, 4, 5, 6], 2)')).toEqual([0.5, 1.5, 2.5, 3.5, 4.5, 5.5])
    expect(lits.run('vec:centered-moving-median([1, 2, 3, 4, 5, 6], 2, 10)')).toEqual([5.5, 1.5, 2.5, 3.5, 4.5, 5.5])
    expect(lits.run('vec:centered-moving-median([1, 2, 3, 4, 5, 6], 3)')).toEqual([1, 2, 3, 4, 5, 5])
    expect(lits.run('vec:centered-moving-median([1, 2, 3, 4, 5, 6], 100, 0, 100)')).toEqual([0.5, 1.5, 2.5, 3.5, 4.5, 5.5])
  })
  it('should calculate the running median of a vector', () => {
    expect(lits.run('vec:running-median([1, 2, 3, 4, 5, 6])')).toEqual([1, 1.5, 2, 2.5, 3, 3.5])
    expect(lits.run('vec:running-median([1, -3, 2])')).toEqual([1, -1, 1])
    expect(lits.run('vec:running-median([-1, -2, -3])')).toEqual([-1, -1.5, -2])
    expect(lits.run('vec:running-median([0])')).toEqual([0])
    expect(() => lits.run('vec:running-median([])')).toThrowError(LitsError)
  })
})
