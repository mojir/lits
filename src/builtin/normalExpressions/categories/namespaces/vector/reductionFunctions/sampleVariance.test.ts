import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('vec:sample-variance', () => {
  it('should calculate the sample variance of a vector', () => {
    expect(lits.run('vec:sample-variance([1, 2, 3])')).toEqual(1)
    expect(lits.run('vec:sample-variance([1, 2, 2, 3])')).toEqual(0.6666666666666666)
    expect(() => lits.run('vec:sample-variance([0])')).toThrowError(LitsError)
    expect(() => lits.run('vec:sample-variance([])')).toThrowError(LitsError)
  })
  it('should calculate the moving sample variance of a vector', () => {
    expect(lits.run('vec:moving-sample-variance([1, 2, 4, 7, 11, 16], 2)')).toEqual([0.5, 2, 4.5, 8, 12.5])
    expect(lits.run('vec:moving-sample-variance([1, 2, 4, 7, 11, 16], 6)')).toEqual([33.366666666666667])
    expect(lits.run('vec:moving-sample-variance([1, 2, 4, 7, 11, 16], 100)')).toEqual([33.366666666666667])
  })
  it('should calculate the centered moving sample variance of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-sample-variance([1, 2, 4], 3)')).toEqual([1, 2.333333333333333, 4])
    expect(lits.run('vec:centered-moving-sample-variance([1, 2, 4], 3, 0, 5)')).toEqual([1, 2.333333333333333, 2.333333333333333])
    expect(() => lits.run('vec:centered-moving-sample-variance([1, 2, 4], 1)')).toThrowError(LitsError)
  })
  it('should calculate the running sample variance of a vector', () => {
    expect(lits.run('vec:running-sample-variance([1, 2, 3])')).toEqual([null, 0.5, 1])
    expect(lits.run('vec:running-sample-variance([0, 1])')).toEqual([null, 0.5])
    expect(() => lits.run('vec:running-sample-variance([2])')).toThrowError(LitsError)
    expect(() => lits.run('vec:running-sample-variance([1])')).toThrowError(LitsError)
  })
})
