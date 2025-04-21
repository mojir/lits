import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('golomb', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:golomb-seq(1)')).toEqual([1])
    expect(lits.run('nth:golomb-seq(2)')).toEqual([1, 2])
    expect(lits.run('nth:golomb-seq(3)')).toEqual([1, 2, 2])
    expect(lits.run('nth:golomb-seq(4)')).toEqual([1, 2, 2, 3])
    expect(lits.run('nth:golomb-seq(84)')).toEqual([
      1,
      2,
      2,
      3,
      3,
      4,
      4,
      4,
      5,
      5,
      5,
      6,
      6,
      6,
      6,
      7,
      7,
      7,
      7,
      8,
      8,
      8,
      8,
      9,
      9,
      9,
      9,
      9,
      10,
      10,
      10,
      10,
      10,
      11,
      11,
      11,
      11,
      11,
      12,
      12,
      12,
      12,
      12,
      12,
      13,
      13,
      13,
      13,
      13,
      13,
      14,
      14,
      14,
      14,
      14,
      14,
      15,
      15,
      15,
      15,
      15,
      15,
      16,
      16,
      16,
      16,
      16,
      16,
      16,
      17,
      17,
      17,
      17,
      17,
      17,
      17,
      18,
      18,
      18,
      18,
      18,
      18,
      18,
      19,
    ])
    expect(() => lits.run('nth:golomb-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:golomb-nth(1)')).toEqual(1)
    expect(lits.run('nth:golomb-nth(2)')).toEqual(2)
    expect(lits.run('nth:golomb-nth(3)')).toEqual(2)
    expect(lits.run('nth:golomb-nth(4)')).toEqual(3)
    expect(lits.run('nth:golomb-nth(5)')).toEqual(3)
    expect(lits.run('nth:golomb-nth(6)')).toEqual(4)
    expect(lits.run('nth:golomb-nth(7)')).toEqual(4)
    expect(lits.run('nth:golomb-nth(8)')).toEqual(4)
    expect(lits.run('nth:golomb-nth(20)')).toEqual(8)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:golomb-take-while(-> $ < 10)')).toEqual([1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 9])
    expect(lits.run('nth:golomb-take-while(-> $2 != 0)')).toEqual([])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:golomb?(0)')).toEqual(true)
    expect(lits.run('nth:golomb?(1)')).toEqual(true)
    expect(lits.run('nth:golomb?(2)')).toEqual(true)
    expect(lits.run('nth:golomb?(3)')).toEqual(true)
    expect(lits.run('nth:golomb?(4)')).toEqual(true)
    expect(lits.run('nth:golomb?(5)')).toEqual(true)
    expect(lits.run('nth:golomb?(6)')).toEqual(true)
    expect(lits.run('nth:golomb?(7)')).toEqual(true)
    expect(lits.run('nth:golomb?(8)')).toEqual(true)
    expect(lits.run('nth:golomb?(100)')).toEqual(true)
    expect(lits.run('nth:golomb?(101)')).toEqual(true)
  })
})
