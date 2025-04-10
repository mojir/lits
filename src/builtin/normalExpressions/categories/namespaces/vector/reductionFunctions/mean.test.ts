import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()

describe('mean', () => {
  it('should calculate the mean of a vector', () => {
    expect(lits.run('vec:mean([1, 2, 3])')).toEqual(2)
    expect(lits.run('vec:mean([1, -3, 2])')).toEqual(0)
    expect(lits.run('vec:mean([-1, -2, -3])')).toEqual(-2)
    expect(lits.run('vec:mean([0])')).toEqual(0)
    expect(() => lits.run('vec:mean([])')).toThrowError(LitsError)
  })
  it('should calculate the moving mean of a vector', () => {
    expect(lits.run('vec:moving-mean([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:moving-mean([1, 2, 3, 4, 5, 6], 3)')).toEqual([2, 3, 4, 5])
    expect(lits.run('vec:moving-mean([1, 2, 3, 4, 5, 6], 6)')).toEqual([3.5])
    expect(lits.run('vec:moving-mean([1, 2, 3, 4, 5, 6], 100)')).toEqual([3.5])
  })
  it('should calculate the centered moving mean of a vector with padding', () => {
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 1)')).toEqual([1, 2, 3, 4, 5, 6])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 2)')).toEqual([0.5, 1.5, 2.5, 3.5, 4.5, 5.5])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 2, 10)')).toEqual([5.5, 1.5, 2.5, 3.5, 4.5, 5.5])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 3)')).toEqual([1, 2, 3, 4, 5, 11 / 3])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 4)')).toEqual([3 / 4, 6 / 4, 10 / 4, 14 / 4, 18 / 4, 15 / 4])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 5)')).toEqual([6 / 5, 2, 3, 4, 18 / 5, 15 / 5])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 6)')).toEqual([6 / 6, 10 / 6, 15 / 6, 21 / 6, 20 / 6, 18 / 6])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 7)')).toEqual([10 / 7, 15 / 7, 21 / 7, 21 / 7, 20 / 7, 18 / 7])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 8)')).toEqual([10 / 8, 15 / 8, 21 / 8, 21 / 8, 21 / 8, 20 / 8])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 9)')).toEqual([15 / 9, 21 / 9, 21 / 9, 21 / 9, 21 / 9, 20 / 9])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 10)')).toEqual([15 / 10, 21 / 10, 21 / 10, 21 / 10, 21 / 10, 21 / 10])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 11)')).toEqual([21 / 11, 21 / 11, 21 / 11, 21 / 11, 21 / 11, 21 / 11])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 12)')).toEqual([21 / 12, 21 / 12, 21 / 12, 21 / 12, 21 / 12, 21 / 12])
    expect(lits.run('vec:centered-moving-mean([1, 2, 3, 4, 5, 6], 100)')).toEqual([21 / 100, 21 / 100, 21 / 100, 21 / 100, 21 / 100, 21 / 100])
  })
  it('should calculate the running mean of a vector', () => {
    expect(lits.run('vec:running-mean([1, 2, 3, 4, 5, 6])')).toEqual([1, 1.5, 2, 2.5, 3, 3.5])
    expect(lits.run('vec:running-mean([1, -3, 2])')).toEqual([1, -1, 0])
    expect(lits.run('vec:running-mean([-1, -2, -3])')).toEqual([-1, -1.5, -2])
    expect(lits.run('vec:running-mean([0])')).toEqual([0])
    expect(() => lits.run('vec:running-mean([])')).toThrowError(LitsError)
  })
})
