import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { vectorNamespace } from '..'

const lits = new Lits({ namespaces: [vectorNamespace] })

// Helper to run vec namespace functions with the new import syntax
function runVec(code: string): unknown {
  // Add namespace import prefix to function calls
  const modifiedCode = `let v = import("Vector"); v.${code}`
  return lits.run(modifiedCode)
}

describe('span', () => {
  it('should calculate the span of a vector', () => {
    expect(runVec('span([1, 2, 3, 4])')).toEqual(3)
    expect(runVec('span([1, 2, 3, 4, 5])')).toEqual(4)
    expect(runVec('span([1])')).toEqual(0)
    expect(runVec('span([])')).toEqual(0)
  })
  it('should calculate the moving span of a vector', () => {
    expect(runVec('moving-span([1, 2, 4, 7, 11, 16], 4)')).toEqual([6, 9, 12])
    expect(runVec('moving-span([1, 2, 4, 7, 11, 16], 5)')).toEqual([10, 14])
    expect(runVec('moving-span([1, 2, 4, 7, 11, 16], 6)')).toEqual([15])
    expect(runVec('moving-span([], 0)')).toEqual([])
  })
  it('should calculate the centered moving span of a vector with padding', () => {
    expect(runVec('centered-moving-span([1, 2, 4, 7, 11], 4)')).toEqual([null, null, 6, 9, null])
    expect(runVec('centered-moving-span([], 0, 0, 100)')).toEqual([])
  })
  it('should calculate the running span of a vector', () => {
    expect(runVec('running-span([1, 2, 3, 4, 5, 6])')).toEqual([0, 1, 2, 3, 4, 5])
    expect(runVec('running-span([-1, -2, -3])')).toEqual([0, 1, 2])
    expect(runVec('running-span([])')).toEqual([])
  })
})
