import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()

describe('vec:min', () => {
  it('should calculate min of a vector', () => {
    expect(lits.run('vec:min([1, 2, 3])')).toEqual(1)
    expect(lits.run('vec:min([1, -2, 3])')).toEqual(-2)
    expect(lits.run('vec:min([-1, -2, -3])')).toEqual(-3)
    expect(lits.run('vec:min([0])')).toEqual(0)
    expect(() => lits.run('vec:min([])')).toThrow()
  })
  it('should calculate the moving min of a vector', () => {
    expect(lits.run('vec:moving-min([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:moving-min([1, 2, 3, 4, 5, 6], 3)')).toEqual([1, 2, 3, 4])
    expect(lits.run('vec:moving-min([1, -2, -3], 2)')).toEqual([-2, -3])
    expect(lits.run('vec:moving-min([1], 100)')).toEqual([1])
    expect(() => lits.run('vec:moving-min([], 1)')).toThrow()
  })
  it('should calculate the centered moving min of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-min([1, 2, 3, 4, 5, 6], 3, 0)')).toEqual([0, 1, 2, 3, 4, 0])
    expect(lits.run('vec:centered-moving-min([1, 2, 3, 4, 5, 6], 3)')).toEqual([1, 1, 2, 3, 4, 5])
    expect(lits.run('vec:centered-moving-min([1, -2, -3], 2)')).toEqual([1, -2, -3])
    expect(lits.run('vec:centered-moving-min([1], 100)')).toEqual([1])
    expect(() => lits.run('vec:centered-moving-min([], 1)')).toThrow()
  })
  it('should calculate the running min of a vector', () => {
    expect(lits.run('vec:running-min([1, 2, 3, 4, 5, 6])')).toEqual([1, 1, 1, 1, 1, 1])
    expect(lits.run('vec:running-min([1, -2, -3])')).toEqual([1, -2, -3])
    expect(lits.run('vec:running-min([1])')).toEqual([1])
    expect(() => lits.run('vec:running-min([], 1)')).toThrow()
  })
})
