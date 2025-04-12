import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()

describe('vec:prod', () => {
  it('should calculate product of a vector', () => {
    expect(lits.run('vec:prod([1, 2, 3])')).toEqual(6)
    expect(lits.run('vec:prod([1, -2, 3])')).toEqual(-6)
    expect(lits.run('vec:prod([-1, -2, -3])')).toEqual(-6)
    expect(lits.run('vec:prod([0])')).toEqual(0)
    expect(lits.run('vec:prod([])')).toEqual(1)
  })
  it('should calculate the moving product of a vector', () => {
    expect(lits.run('vec:moving-prod([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:moving-prod([1, 2, 3, 4, 5, 6], 3)')).toEqual([6, 24, 60, 120])
    expect(lits.run('vec:moving-prod([1, -2, -3], 2)')).toEqual([-2, 6])
    expect(lits.run('vec:moving-prod([], 0)')).toEqual([])
  })
  it('should calculate the centered moving product of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-prod([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:centered-moving-prod([1, 2, 3, 4, 5, 6], 3)')).toEqual([2, 6, 24, 60, 120, 30])
    expect(lits.run('vec:centered-moving-prod([], 0)')).toEqual([])
  })
  it('should calculate the running product of a vector', () => {
    expect(lits.run('vec:running-prod([1, 2, 3, 4, 5, 6])')).toEqual([1, 2, 6, 24, 120, 720])
    expect(lits.run('vec:running-prod([1, -2, -3])')).toEqual([1, -2, 6])
    expect(lits.run('vec:running-prod([1])')).toEqual([1])
    expect(lits.run('vec:running-prod([])')).toEqual([])
  })
})
