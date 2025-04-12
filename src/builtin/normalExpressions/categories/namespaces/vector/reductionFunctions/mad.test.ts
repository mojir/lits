import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('vec:mad', () => {
  it('should calculate the mean absolute deviation of a vector', () => {
    expect(lits.run('vec:mad([1, 2, 3])')).toEqual(0.6666666666666666)
    expect(lits.run('vec:mad([1, 2, 2, 3])')).toEqual(0.5)
    expect(() => lits.run('vec:mad([])')).toThrowError(LitsError)
  })
  it('should calculate the moving mean absolute deviation of a vector', () => {
    expect(lits.run('vec:moving-mad([1, 2, 3, 4, 5, 6], 1)')).toEqual([0, 0, 0, 0, 0, 0])
    expect(lits.run('vec:moving-mad([1, 2, 4, 7, 11], 3)')).toEqual([1, 1.6666666666666667, 2.3333333333333335])
    expect(lits.run('vec:moving-mad([1, -2, -3], 2)')).toEqual([1.5, 0.5])
    expect(() => lits.run('vec:moving-mad([1], 100)')).toThrow(LitsError)
    expect(() => lits.run('vec:moving-mad([], 1)')).toThrowError(LitsError)
  })
  it('should calculate the centered moving mean absolute deviation of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-mad([1, 2, 3, 4, 5], 3)')).toEqual([null, 0.6666666666666666, 0.6666666666666666, 0.6666666666666666, null])
    expect(lits.run('vec:centered-moving-mad([1, -2, -3], 2)')).toEqual([null, 1.5, 0.5])
    expect(() => lits.run('vec:centered-moving-mad([1], 100)')).toThrow(LitsError)
    expect(() => lits.run('vec:centered-moving-mad([], 1)')).toThrowError(LitsError)
  })
  it('should calculate the running mean absolute deviation of a vector', () => {
    expect(lits.run('vec:running-mad([1, 2, 3])')).toEqual([0, 0.5, 0.6666666666666666])
    expect(lits.run('vec:running-mad([1, -2, -3])')).toEqual([0, 1.5, 1.3333333333333333])
    expect(() => lits.run('vec:running-mad([])')).toThrowError(LitsError)
  })
})
