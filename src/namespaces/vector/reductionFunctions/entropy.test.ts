import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { LitsError } from '../../../errors'

const lits = new Lits()

// Helper to run vec namespace functions with the new import syntax
function runVec(code: string): unknown {
  // Add namespace import prefix to function calls
  const modifiedCode = `let v = import("Vector"); v.${code}`
  return lits.run(modifiedCode)
}

describe('iqr', () => {
  it('should calculate the entropy of a vector', () => {
    expect(runVec('entropy([1, 1, 2, 3, 3, 3])')).toBe(1.4591479170272448)
    expect(runVec('entropy([1, 2, 3])')).toEqual(1.584962500721156)
    expect(runVec('entropy([1, 2, 2, 3])')).toEqual(1.5)
    expect(runVec('entropy([0])')).toEqual(0)
    expect(() => runVec('entropy([])')).toThrowError(LitsError)
  })
  it('should calculate the moving entropy of a vector', () => {
    expect(runVec('moving-entropy([1, 1, 2, 3, 3, 3], 4)')).toEqual([1.5, 1.5, 0.8112781244591328])
    expect(runVec('moving-entropy([1, 1, 2, 3, 3, 3], 3)')).toEqual([0.9182958340544896, 1.584962500721156, 0.9182958340544896, 0])
    expect(runVec('moving-entropy([1, 2], 2)')).toEqual([1])
    expect(() => runVec('moving-entropy([], 3)')).toThrowError(LitsError)
  })
  it('should calculate the centered moving entropy of a vector with padding', () => {
    expect(runVec('centered-moving-entropy([1, 1, 2, 3, 3, 3], 4)')).toEqual([null, null, 1.5, 1.5, 0.8112781244591328, null])
    expect(runVec('centered-moving-entropy([1, 1, 2, 3, 3, 3], 3)')).toEqual([null, 0.9182958340544896, 1.584962500721156, 0.9182958340544896, 0, null])
    expect(runVec('centered-moving-entropy([1, 2], 2)')).toEqual([null, 1])
  })
  it('should calculate the running entropy of a vector', () => {
    expect(runVec('running-entropy([1, 1, 2, 3, 3, 3])')).toEqual([0, 0, 0.9182958340544896, 1.5, 1.5219280948873621, 1.4591479170272448])
    expect(runVec('running-entropy([1, 2])')).toEqual([0, 1])
    expect(() => runVec('running-entropy([])')).toThrowError(LitsError)
  })
})
