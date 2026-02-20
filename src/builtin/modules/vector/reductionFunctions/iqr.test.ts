import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { vectorModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [vectorModule] })

// Helper to run vec module functions with the new import syntax
function runVec(code: string): unknown {
  // Add module import prefix to function calls
  const modifiedCode = `let v = import("vector"); v.${code}`
  return lits.run(modifiedCode)
}

describe('iqr', () => {
  it('should calculate the interquartile range of a vector', () => {
    expect(runVec('iqr([1, 2, 3, 4])')).toEqual(2)
    expect(runVec('iqr([1, 2, 3, 4, 5])')).toEqual(3)
    expect(() => runVec('iqr([1])')).toThrowError(LitsError)
    expect(() => runVec('iqr([1, 2, 3])')).toThrowError(LitsError)
    expect(() => runVec('iqr([])')).toThrowError(LitsError)
  })
  it('should calculate the moving interquartile range of a vector', () => {
    expect(runVec('moving-iqr([1, 2, 4, 7, 11, 16], 4)')).toEqual([4, 6, 8])
    expect(runVec('moving-iqr([1, 2, 4, 7, 11, 16], 5)')).toEqual([7.5, 10.5])
    expect(runVec('moving-iqr([1, 2, 4, 7, 11, 16], 6)')).toEqual([9])
  })
  it('should calculate the centered moving interquartile range of a vector with padding', () => {
    expect(runVec('centered-moving-iqr([1, 2, 4, 7, 11], 4)')).toEqual([null, null, 4, 6, null])
    expect(runVec('centered-moving-iqr([1, 2, 4, 7, 11], 5, 0, 100)')).toEqual([3, 5, 7.5, 52.5, 94.5])
  })
  it('should calculate the running interquartile range of a vector', () => {
    expect(runVec('running-iqr([1, 2, 3, 4, 5, 6])')).toEqual([null, null, null, 2, 3, 3])
    expect(() => runVec('running-iqr([-1, -2, -3])')).toThrow(LitsError)
    expect(() => runVec('running-iqr([])')).toThrowError(LitsError)
  })
})
