import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { vectorModule } from '..'

const lits = new Lits({ modules: [vectorModule] })

// Helper to run vec module functions with the new import syntax
function runVec(code: string): unknown {
  // Add module import prefix to function calls
  const modifiedCode = `let v = import(vector); v.${code}`
  return lits.run(modifiedCode)
}

describe('prod', () => {
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
