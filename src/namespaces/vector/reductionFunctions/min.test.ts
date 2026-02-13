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

describe('min', () => {
  it('should calculate min of a vector', () => {
    expect(runVec('TEMP-min([1, 2, 3])')).toEqual(1)
    expect(runVec('TEMP-min([1, -2, 3])')).toEqual(-2)
    expect(runVec('TEMP-min([-1, -2, -3])')).toEqual(-3)
    expect(runVec('TEMP-min([0])')).toEqual(0)
    expect(() => runVec('TEMP-min([])')).toThrow(LitsError)
  })
  it('should calculate the moving min of a vector', () => {
    expect(runVec('moving-min([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(runVec('moving-min([1, 2, 3, 4, 5, 6], 3)')).toEqual([1, 2, 3, 4])
    expect(runVec('moving-min([1, -2, -3], 2)')).toEqual([-2, -3])
    expect(() => runVec('moving-min([], 0)')).toThrow(LitsError)
  })
  it('should calculate the centered moving min of a vector with padding', () => {
    expect(runVec('centered-moving-min([1, 2, 3, 4, 5, 6], 3, 0)')).toEqual([0, 1, 2, 3, 4, 5])
    expect(runVec('centered-moving-min([1, 2, 3, 4, 5, 6], 3)')).toEqual([1, 1, 2, 3, 4, 5])
    expect(runVec('centered-moving-min([1, -2, -3], 2)')).toEqual([1, -2, -3])
    expect(runVec('centered-moving-min([1], 1)')).toEqual([1])
    expect(() => runVec('centered-moving-min([], 1)')).toThrow(LitsError)
  })
  it('should calculate the running min of a vector', () => {
    expect(runVec('running-min([1, 2, 3, 4, 5, 6])')).toEqual([1, 1, 1, 1, 1, 1])
    expect(runVec('running-min([1, -2, -3])')).toEqual([1, -2, -3])
    expect(runVec('running-min([1])')).toEqual([1])
    expect(() => runVec('running-min([], 1)')).toThrow(LitsError)
  })
})
