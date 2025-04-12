import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('vec:iqr', () => {
  it('should calculate the entropy of a vector', () => {
    expect(lits.run('vec:entropy([1, 1, 2, 3, 3, 3])')).toBe(1.4591479170272448)
    expect(lits.run('vec:entropy([1, 2, 3])')).toEqual(1.584962500721156)
    expect(lits.run('vec:entropy([1, 2, 2, 3])')).toEqual(1.5)
    expect(lits.run('vec:entropy([0])')).toEqual(0)
    expect(() => lits.run('vec:entropy([])')).toThrowError(LitsError)
  })
  it('should calculate the moving entropy of a vector', () => {
    expect(lits.run('vec:moving-entropy([1, 1, 2, 3, 3, 3], 4)')).toEqual([1.5, 1.5, 0.8112781244591328])
    expect(lits.run('vec:moving-entropy([1, 1, 2, 3, 3, 3], 3)')).toEqual([0.9182958340544896, 1.584962500721156, 0.9182958340544896, 0])
    expect(lits.run('vec:moving-entropy([1, 2], 2)')).toEqual([1])
    expect(() => lits.run('vec:moving-entropy([], 3)')).toThrowError(LitsError)
  })
  it('should calculate the centered moving entropy of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-entropy([1, 1, 2, 3, 3, 3], 4)')).toEqual([null, null, 1.5, 1.5, 0.8112781244591328, null])
    expect(lits.run('vec:centered-moving-entropy([1, 1, 2, 3, 3, 3], 3)')).toEqual([null, 0.9182958340544896, 1.584962500721156, 0.9182958340544896, 0, null])
    expect(lits.run('vec:centered-moving-entropy([1, 2], 2)')).toEqual([null, 1])
  })
  it('should calculate the running entropy of a vector', () => {
    expect(lits.run('vec:running-entropy([1, 1, 2, 3, 3, 3])')).toEqual([0, 0, 0.9182958340544896, 1.5, 1.5219280948873621, 1.4591479170272448])
    expect(lits.run('vec:running-entropy([1, 2])')).toEqual([0, 1])
    expect(() => lits.run('vec:running-entropy([])')).toThrowError(LitsError)
  })
})
