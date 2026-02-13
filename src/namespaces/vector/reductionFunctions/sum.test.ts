import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'

const lits = new Lits()

// Helper to run vec namespace functions with the new import syntax
function runVec(code: string): unknown {
  // Add namespace import prefix to function calls
  const modifiedCode = `let v = import("Vector"); v.${code}`
  return lits.run(modifiedCode)
}

describe('sum', () => {
  it('should calculate sum of a vector', () => {
    expect(runVec('sum([1, 2, 3])')).toEqual(6)
    expect(runVec('sum([1, -2, 3])')).toEqual(2)
    expect(runVec('sum([-1, -2, -3])')).toEqual(-6)
    expect(runVec('sum([0])')).toEqual(0)
    expect(runVec('sum([])')).toEqual(0)
  })
  it('should calculate the moving sum of a vector', () => {
    expect(runVec('moving-sum([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(runVec('moving-sum([1, 2, 3, 4, 5, 6], 3)')).toEqual([6, 9, 12, 15])
    expect(runVec('moving-sum([1, 2, 3, 4, 5, 6], 6)')).toEqual([21])
    expect(runVec('moving-sum([1, -2, -3], 2)')).toEqual([-1, -5])
    expect(runVec('moving-sum([], 0)')).toEqual([])
  })
  it('should calculate the centered moving sum of a vector with padding', () => {
    expect(runVec('centered-moving-sum([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(runVec('centered-moving-sum([1, 2, 3, 4, 5, 6], 2)')).toEqual([null, 3, 5, 7, 9, 11])
    expect(runVec('centered-moving-sum([1, 2, 3, 4, 5, 6], 2, 10)')).toEqual([11, 3, 5, 7, 9, 11])
    expect(runVec('centered-moving-sum([1, 2, 3, 4, 5, 6], 3)')).toEqual([null, 6, 9, 12, 15, null])
    expect(runVec('centered-moving-sum([1, 2, 3, 4, 5, 6], 6)')).toEqual([null, null, null, 21, null, null])
    expect(runVec('centered-moving-sum([], 0)')).toEqual([])
  })
  it('should calculate the running sum of a vector', () => {
    expect(runVec('running-sum([1, 2, 3, 4, 5, 6])')).toEqual([1, 3, 6, 10, 15, 21])
    expect(runVec('running-sum([1, -2, -3])')).toEqual([1, -1, -4])
    expect(runVec('running-sum([-1, -2, -3])')).toEqual([-1, -3, -6])
    expect(runVec('running-sum([0])')).toEqual([0])
    expect(runVec('running-sum([])')).toEqual([])
  })
})
