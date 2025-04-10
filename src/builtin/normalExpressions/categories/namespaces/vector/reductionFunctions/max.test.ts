import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()

describe('vec:max', () => {
  it('should calculate max of a vector', () => {
    expect(lits.run('vec:max([1, 2, 3])')).toEqual(3)
    expect(lits.run('vec:max([1, -2, 3])')).toEqual(3)
    expect(lits.run('vec:max([-1, -2, -3])')).toEqual(-1)
    expect(lits.run('vec:max([0])')).toEqual(0)
    expect(() => lits.run('vec:max([])')).toThrow()
  })
  it('should calculate the moving max of a vector', () => {
    expect(lits.run('vec:moving-max([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:moving-max([1, 2, 3, 4, 5, 6], 3)')).toEqual([3, 4, 5, 6])
    expect(lits.run('vec:moving-max([1, -2, -3], 2)')).toEqual([1, -2])
    expect(lits.run('vec:moving-max([1], 100)')).toEqual([1])
    expect(() => lits.run('vec:moving-max([], 1)')).toThrow()
  })
  it('should calculate the centered moving max of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-max([1, 2, 3, 4, 5, 6], 3, 0, 100)')).toEqual([2, 3, 4, 5, 6, 100])
    expect(lits.run('vec:centered-moving-max([1, 2, 3, 4, 5, 6], 3)')).toEqual([2, 3, 4, 5, 6, 6])
    expect(lits.run('vec:centered-moving-max([1, -2, -3], 2)')).toEqual([1, 1, -2])
    expect(lits.run('vec:centered-moving-max([1], 100)')).toEqual([1])
    expect(() => lits.run('vec:centered-moving-max([], 1)')).toThrow()
  })
  it('should calculate the running max of a vector', () => {
    expect(lits.run('vec:running-max([1, 2, 3, 4, 5, 6])')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:running-max([1, -2, -3])')).toEqual([1, 1, 1])
    expect(lits.run('vec:running-max([1])')).toEqual([1])
    expect(() => lits.run('vec:running-max([], 1)')).toThrow()
  })
})
