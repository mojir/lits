import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('deficient', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('c:deficient-seq(1)')).toEqual([1])
    expect(lits.run('c:deficient-seq(2)')).toEqual([1, 2])
    expect(lits.run('c:deficient-seq(3)')).toEqual([1, 2, 3])
    expect(lits.run('c:deficient-seq(18)')).toEqual([1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 19, 21, 22])
    expect(() => lits.run('c:deficient-seq(0)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('c:deficient-nth(1)')).toEqual(1)
    expect(lits.run('c:deficient-nth(2)')).toEqual(2)
    expect(lits.run('c:deficient-nth(3)')).toEqual(3)
    expect(lits.run('c:deficient-nth(4)')).toEqual(4)
    expect(lits.run('c:deficient-nth(20)')).toEqual(25)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('c:deficient-take-while(-> $ < 20)')).toEqual([
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
    expect(lits.run('c:deficient?(0)')).toEqual(false)
    expect(lits.run('c:deficient?(1)')).toEqual(true)
    expect(lits.run('c:deficient?(2)')).toEqual(true)
    expect(lits.run('c:deficient?(3)')).toEqual(true)
    expect(lits.run('c:deficient?(12)')).toEqual(false)
    expect(lits.run('c:deficient?(15)')).toEqual(true)
    expect(lits.run('c:deficient?(18)')).toEqual(false)
  })
})
