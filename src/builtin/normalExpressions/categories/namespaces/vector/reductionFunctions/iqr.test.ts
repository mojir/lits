import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('vec:iqr', () => {
  it('should calculate the interquartile range of a vector', () => {
    expect(lits.run('vec:iqr([1, 2, 3, 4])')).toEqual(2)
    expect(lits.run('vec:iqr([1, 2, 3, 4, 5])')).toEqual(3)
    expect(() => lits.run('vec:iqr([1])')).toThrowError(LitsError)
    expect(() => lits.run('vec:iqr([1, 2, 3])')).toThrowError(LitsError)
    expect(() => lits.run('vec:iqr([])')).toThrowError(LitsError)
  })
  it('should calculate the moving interquartile range of a vector', () => {
    expect(lits.run('vec:moving-iqr([1, 2, 4, 7, 11, 16], 4)')).toEqual([4, 6, 8])
    expect(lits.run('vec:moving-iqr([1, 2, 4, 7, 11, 16], 5)')).toEqual([7.5, 10.5])
    expect(lits.run('vec:moving-iqr([1, 2, 4, 7, 11, 16], 6)')).toEqual([9])
  })
  it('should calculate the centered moving interquartile range of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-iqr([1, 2, 4, 7, 11], 4)')).toEqual([null, null, 4, 6, null])
    expect(lits.run('vec:centered-moving-iqr([1, 2, 4, 7, 11], 5, 0, 100)')).toEqual([3, 5, 7.5, 52.5, 94.5])
  })
  it('should calculate the running interquartile range of a vector', () => {
    expect(lits.run('vec:running-iqr([1, 2, 3, 4, 5, 6])')).toEqual([null, null, null, 2, 3, 3])
    expect(() => lits.run('vec:running-iqr([-1, -2, -3])')).toThrow(LitsError)
    expect(() => lits.run('vec:running-iqr([])')).toThrowError(LitsError)
  })
})
