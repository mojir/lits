import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('vec:gini-coefficient', () => {
  it('should calculate gini-coefficient of a vector', () => {
    expect(lits.run('vec:gini-coefficient([1, 2, 3])')).toEqual(0.22222222222222232)
    expect(lits.run('vec:gini-coefficient([1, 1, 3])')).toEqual(0.26666666666666683)
    expect(lits.run('vec:gini-coefficient([0, 0, 0])')).toEqual(0)
    expect(() => lits.run('vec:gini-coefficient([-1, 0, 0])')).toThrow()
    expect(() => lits.run('vec:gini-coefficient([])')).toThrowError(LitsError)
  })
  it('should calculate the moving gini-coefficient of a vector', () => {
    expect(lits.run('vec:moving-gini-coefficient([1, 2, 3], 2)')).toEqual([0.16666666666666674, 0.10000000000000009])
    expect(lits.run('vec:moving-gini-coefficient([1, 1, 3], 2)')).toEqual([0, 0.25])
    expect(lits.run('vec:moving-gini-coefficient([0, 0, 0], 2)')).toEqual([0, 0])
    expect(() => lits.run('vec:moving-gini-coefficient([], 2)')).toThrowError(LitsError)
  })
  it('should calculate the centered moving gini-coefficient of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-gini-coefficient([1, 2, 3], 2)')).toEqual([null, 0.16666666666666674, 0.10000000000000009])
    expect(lits.run('vec:centered-moving-gini-coefficient([1, 1, 3], 2)')).toEqual([null, 0, 0.25])
    expect(lits.run('vec:centered-moving-gini-coefficient([0, 0, 0], 2)')).toEqual([null, 0, 0])
    expect(() => lits.run('vec:centered-moving-gini-coefficient([], 2)')).toThrowError(LitsError)
  })
  it('should calculate the running gini-coefficient of a vector', () => {
    expect(lits.run('vec:running-gini-coefficient([1, 2, 3])')).toEqual([0, 0.16666666666666674, 0.22222222222222232])
    expect(lits.run('vec:running-gini-coefficient([1, 1, 3])')).toEqual([0, 0, 0.26666666666666683])
    expect(lits.run('vec:running-gini-coefficient([0, 0, 0])')).toEqual([0, 0, 0])
    expect(() => lits.run('vec:running-gini-coefficient([])')).toThrowError(LitsError)
  })
})
