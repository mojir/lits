import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('vec:variance', () => {
  it('should calculate the variance of a vector', () => {
    expect(lits.run('vec:variance([1, 2, 3])')).toEqual(0.6666666666666666)
    expect(lits.run('vec:variance([1, 2, 3, 4, 5, 6])')).toEqual(2.9166666666666665)
    expect(lits.run('vec:variance([1, -3, 2])')).toEqual(4.666666666666667)
    expect(lits.run('vec:variance([-1, -2, -3])')).toEqual(0.6666666666666666)
    expect(lits.run('vec:variance([0])')).toEqual(0)
    expect(() => lits.run('vec:variance([])')).toThrowError(LitsError)
  })
  it('should calculate the moving variance of a vector', () => {
    expect(lits.run('vec:moving-variance([1, 2, 4, 7, 11, 16], 1)')).toEqual([0, 0, 0, 0, 0, 0])
    expect(lits.run('vec:moving-variance([1, 2, 4, 7, 11, 16], 4)')).toEqual([5.25, 11.5, 20.25])
    expect(lits.run('vec:moving-variance([1, 2, 4, 7, 11, 16], 6)')).toEqual([27.805555555555557])
    expect(lits.run('vec:moving-variance([1, 2, 4, 7, 11, 16], 100)')).toEqual([27.805555555555557])
  })
  it('should calculate the centered moving variance of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-variance([1, 2, 4, 7, 11, 16], 4)')).toEqual([0.6875, 2.1875, 5.25, 11.5, 20.25, 34.25])
    expect(lits.run('vec:centered-moving-variance([1, 2, 4, 7, 11, 16], 6)')).toEqual([2.138888888888889, 6.222222222222221, 14.472222222222223, 27.805555555555557, 29.888888888888886, 33.555555555555564])
  })
  it('should calculate the running variance of a vector', () => {
    expect(lits.run('vec:running-variance([1, 2, 3, 4, 5, 6])')).toEqual([0, 0.25, 0.6666666666666666, 1.25, 2, 2.9166666666666665])
    expect(lits.run('vec:running-variance([0])')).toEqual([0])
    expect(() => lits.run('vec:running-variance([])')).toThrowError(LitsError)
  })
})
