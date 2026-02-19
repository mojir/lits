import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { vectorModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [vectorModule] })

// Helper to run vec module functions with the new import syntax
function runVec(code: string): unknown {
  // Add module import prefix to function calls
  const modifiedCode = `let v = import("Vector"); v.${code}`
  return lits.run(modifiedCode)
}

describe('max', () => {
  it('should calculate max of a vector', () => {
    expect(runVec('max([1, 2, 3])')).toEqual(3)
    expect(runVec('max([1, -2, 3])')).toEqual(3)
    expect(runVec('max([-1, -2, -3])')).toEqual(-1)
    expect(runVec('max([0])')).toEqual(0)
    expect(() => runVec('max([])')).toThrowError(LitsError)
  })
  it('should calculate the moving max of a vector', () => {
    expect(runVec('moving-max([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(runVec('moving-max([1, 2, 3, 4, 5, 6], 3)')).toEqual([3, 4, 5, 6])
    expect(runVec('moving-max([1, -2, -3], 2)')).toEqual([1, -2])
    expect(() => runVec('moving-max([], 1)')).toThrowError(LitsError)
  })
  it('should calculate the centered moving max of a vector with padding', () => {
    expect(runVec('centered-moving-max([1, 2, 3, 4, 5, 6], 3, 0, 100)')).toEqual([2, 3, 4, 5, 6, 100])
    expect(runVec('centered-moving-max([1, 2, 3, 4, 5, 6], 3)')).toEqual([2, 3, 4, 5, 6, 6])
    expect(runVec('centered-moving-max([1, -2, -3], 2)')).toEqual([1, 1, -2])
    expect(runVec('centered-moving-max([1], 1)')).toEqual([1])
    expect(() => runVec('centered-moving-max([], 0)')).toThrowError(LitsError)
  })
  it('should calculate the running max of a vector', () => {
    expect(runVec('running-max([1, 2, 3, 4, 5, 6])')).toEqual([1, 2, 3, 4, 5, 6])
    expect(runVec('running-max([1, -2, -3])')).toEqual([1, 1, 1])
    expect(runVec('running-max([1])')).toEqual([1])
    expect(() => runVec('running-max([], 1)')).toThrowError(LitsError)
  })
})
