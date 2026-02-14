import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { LitsError } from '../../../../errors'

const lits = new Lits()

// Helper to run vec namespace functions with the new import syntax
function runVec(code: string): unknown {
  // Add namespace import prefix to function calls
  const modifiedCode = `let v = import("Vector"); v.${code}`
  return lits.run(modifiedCode)
}

describe('gini-coefficient', () => {
  it('should calculate gini-coefficient of a vector', () => {
    expect(runVec('gini-coefficient([1, 2, 3])')).toEqual(0.22222222222222232)
    expect(runVec('gini-coefficient([1, 1, 3])')).toEqual(0.26666666666666683)
    expect(runVec('gini-coefficient([0, 0, 0])')).toEqual(0)
    expect(() => runVec('gini-coefficient([-1, 0, 0])')).toThrow(LitsError)
    expect(() => runVec('gini-coefficient([])')).toThrowError(LitsError)
  })
  it('should calculate the moving gini-coefficient of a vector', () => {
    expect(runVec('moving-gini-coefficient([1, 2, 3], 2)')).toEqual([0.16666666666666674, 0.10000000000000009])
    expect(runVec('moving-gini-coefficient([1, 1, 3], 2)')).toEqual([0, 0.25])
    expect(runVec('moving-gini-coefficient([0, 0, 0], 2)')).toEqual([0, 0])
    expect(() => runVec('moving-gini-coefficient([], 2)')).toThrowError(LitsError)
  })
  it('should calculate the centered moving gini-coefficient of a vector with padding', () => {
    expect(runVec('centered-moving-gini-coefficient([1, 2, 3], 2)')).toEqual([null, 0.16666666666666674, 0.10000000000000009])
    expect(runVec('centered-moving-gini-coefficient([1, 1, 3], 2)')).toEqual([null, 0, 0.25])
    expect(runVec('centered-moving-gini-coefficient([0, 0, 0], 2)')).toEqual([null, 0, 0])
    expect(() => runVec('centered-moving-gini-coefficient([], 2)')).toThrowError(LitsError)
  })
  it('should calculate the running gini-coefficient of a vector', () => {
    expect(runVec('running-gini-coefficient([1, 2, 3])')).toEqual([0, 0.16666666666666674, 0.22222222222222232])
    expect(runVec('running-gini-coefficient([1, 1, 3])')).toEqual([0, 0, 0.26666666666666683])
    expect(runVec('running-gini-coefficient([0, 0, 0])')).toEqual([0, 0, 0])
    expect(() => runVec('running-gini-coefficient([])')).toThrowError(LitsError)
  })
})
