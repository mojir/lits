import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('deficient', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:deficient-seq(1)')).toEqual([1])
    expect(lits.run('nth:deficient-seq(2)')).toEqual([1, 2])
    expect(lits.run('nth:deficient-seq(3)')).toEqual([1, 2, 3])
    expect(lits.run('nth:deficient-seq(18)')).toEqual([1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 19, 21, 22])
    expect(() => lits.run('nth:deficient-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:deficient-nth(1)')).toEqual(1)
    expect(lits.run('nth:deficient-nth(2)')).toEqual(2)
    expect(lits.run('nth:deficient-nth(3)')).toEqual(3)
    expect(lits.run('nth:deficient-nth(4)')).toEqual(4)
    expect(lits.run('nth:deficient-nth(20)')).toEqual(25)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:deficient-take-while(-> $ < 20)')).toEqual([
      1,
      2,
      3,
      4,
      5,
      7,
      8,
      9,
      10,
      11,
      13,
      14,
      15,
      16,
      17,
      19,
    ])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:deficient?(0)')).toEqual(false)
    expect(lits.run('nth:deficient?(1)')).toEqual(true)
    expect(lits.run('nth:deficient?(2)')).toEqual(true)
    expect(lits.run('nth:deficient?(3)')).toEqual(true)
    expect(lits.run('nth:deficient?(12)')).toEqual(false)
    expect(lits.run('nth:deficient?(15)')).toEqual(true)
    expect(lits.run('nth:deficient?(18)')).toEqual(false)
  })
})
