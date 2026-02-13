import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'

const lits = new Lits()

// Helper to run vec namespace functions with the new import syntax
function runVec(code: string): unknown {
  // Add namespace import prefix to function calls
  const modifiedCode = `let v = import("Vector"); v.${code}`
  return lits.run(modifiedCode)
}

describe('prod', () => {
  it('should calculate product of a vector', () => {
    expect(runVec('prod([1, 2, 3])')).toEqual(6)
    expect(runVec('prod([1, -2, 3])')).toEqual(-6)
    expect(runVec('prod([-1, -2, -3])')).toEqual(-6)
    expect(runVec('prod([0])')).toEqual(0)
    expect(runVec('prod([])')).toEqual(1)
  })
  it('should calculate the moving product of a vector', () => {
    expect(runVec('moving-prod([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(runVec('moving-prod([1, 2, 3, 4, 5, 6], 3)')).toEqual([6, 24, 60, 120])
    expect(runVec('moving-prod([1, -2, -3], 2)')).toEqual([-2, 6])
    expect(runVec('moving-prod([], 0)')).toEqual([])
  })
  it('should calculate the centered moving product of a vector with padding', () => {
    expect(runVec('centered-moving-prod([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(runVec('centered-moving-prod([1, 2, 3, 4, 5, 6], 3)')).toEqual([2, 6, 24, 60, 120, 30])
    expect(runVec('centered-moving-prod([], 0)')).toEqual([])
  })
  it('should calculate the running product of a vector', () => {
    expect(runVec('running-prod([1, 2, 3, 4, 5, 6])')).toEqual([1, 2, 6, 24, 120, 720])
    expect(runVec('running-prod([1, -2, -3])')).toEqual([1, -2, 6])
    expect(runVec('running-prod([1])')).toEqual([1])
    expect(runVec('running-prod([])')).toEqual([])
  })
})
