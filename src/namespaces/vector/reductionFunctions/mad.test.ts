import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { LitsError } from '../../../errors'

const lits = new Lits()

// Helper to run vec namespace functions with the new import syntax
function runVec(code: string): unknown {
  // Add namespace import prefix to function calls
  const modifiedCode = `let v = import("vec"); v.${code}`
  return lits.run(modifiedCode)
}

describe('mad', () => {
  it('should calculate the mean absolute deviation of a vector', () => {
    expect(runVec('mad([1, 2, 3])')).toEqual(0.6666666666666666)
    expect(runVec('mad([1, 2, 2, 3])')).toEqual(0.5)
    expect(() => runVec('mad([])')).toThrowError(LitsError)
  })
  it('should calculate the moving mean absolute deviation of a vector', () => {
    expect(runVec('moving-mad([1, 2, 3, 4, 5, 6], 1)')).toEqual([0, 0, 0, 0, 0, 0])
    expect(runVec('moving-mad([1, 2, 4, 7, 11], 3)')).toEqual([1, 1.6666666666666667, 2.3333333333333335])
    expect(runVec('moving-mad([1, -2, -3], 2)')).toEqual([1.5, 0.5])
    expect(() => runVec('moving-mad([1], 100)')).toThrow(LitsError)
    expect(() => runVec('moving-mad([], 1)')).toThrowError(LitsError)
  })
  it('should calculate the centered moving mean absolute deviation of a vector with padding', () => {
    expect(runVec('centered-moving-mad([1, 2, 3, 4, 5], 3)')).toEqual([null, 0.6666666666666666, 0.6666666666666666, 0.6666666666666666, null])
    expect(runVec('centered-moving-mad([1, -2, -3], 2)')).toEqual([null, 1.5, 0.5])
    expect(() => runVec('centered-moving-mad([1], 100)')).toThrow(LitsError)
    expect(() => runVec('centered-moving-mad([], 1)')).toThrowError(LitsError)
  })
  it('should calculate the running mean absolute deviation of a vector', () => {
    expect(runVec('running-mad([1, 2, 3])')).toEqual([0, 0.5, 0.6666666666666666])
    expect(runVec('running-mad([1, -2, -3])')).toEqual([0, 1.5, 1.3333333333333333])
    expect(() => runVec('running-mad([])')).toThrowError(LitsError)
  })
})
