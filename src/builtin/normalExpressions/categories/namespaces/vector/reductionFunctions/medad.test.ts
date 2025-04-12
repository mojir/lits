import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('vec:medad', () => {
  it('should calculate the median absolute deviation of a vector', () => {
    expect(lits.run('vec:medad([1, 2, 3])')).toEqual(1.4826)
    expect(lits.run('vec:medad([1, 2, 2, 3, 5, 15, 50])')).toEqual(2.9652)
    expect(() => lits.run('vec:medad([])')).toThrowError(LitsError)
  })
  it('should calculate the moving median absolute deviation of a vector', () => {
    expect(lits.run('vec:moving-medad([1, 2, 3, 4, 5, 6], 1)')).toEqual([0, 0, 0, 0, 0, 0])
    expect(lits.run('vec:moving-medad([1, 2, 4, 7, 11], 3)')).toEqual([1.4826, 2.9652, 4.4478])
    expect(lits.run('vec:moving-medad([1, -2, -3], 2)')).toEqual([2.2239, 0.7413])
    expect(() => lits.run('vec:moving-medad([1], 100)')).toThrow(LitsError)
    expect(() => lits.run('vec:moving-medad([], 1)')).toThrowError(LitsError)
  })
  it('should calculate the centered moving median absolute deviation of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-medad([1, 2, 3, 4, 5], 3)')).toEqual([null, 1.4826, 1.4826, 1.4826, null])
    expect(lits.run('vec:centered-moving-medad([1, -2, -3], 2)')).toEqual([null, 2.2239, 0.7413])
    expect(() => lits.run('vec:centered-moving-medad([1], 100)')).toThrow(LitsError)
    expect(() => lits.run('vec:centered-moving-medad([], 1)')).toThrowError(LitsError)
  })
  it('should calculate the running median absolute deviation of a vector', () => {
    expect(lits.run('vec:running-medad([1, 2, 3])')).toEqual([0, 0.7413, 1.4826])
    expect(lits.run('vec:running-medad([1, -2, -3])')).toEqual([0, 2.2239, 1.4826])
    expect(() => lits.run('vec:running-medad([])')).toThrowError(LitsError)
  })
})
