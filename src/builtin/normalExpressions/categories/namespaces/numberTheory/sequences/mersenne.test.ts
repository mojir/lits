import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()
describe('mersenne', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('n:mersenne-seq(1)')).toEqual([3])
    expect(lits.run('n:mersenne-seq(2)')).toEqual([3, 7])
    expect(lits.run('n:mersenne-seq(3)')).toEqual([3, 7, 31])
    expect(lits.run('n:mersenne-seq(4)')).toEqual([3, 7, 31, 127])
    expect(lits.run('n:mersenne-seq(9)')).toEqual([3, 7, 31, 127, 2047, 8191, 131071, 524287, 2147483647])
    expect(() => lits.run('n:mersenne-seq(0)')).toThrow()
    expect(() => lits.run('n:mersenne-seq(20)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('n:mersenne-nth(1)')).toEqual(3)
    expect(lits.run('n:mersenne-nth(2)')).toEqual(7)
    expect(lits.run('n:mersenne-nth(3)')).toEqual(31)
    expect(lits.run('n:mersenne-nth(4)')).toEqual(127)
    expect(lits.run('n:mersenne-nth(9)')).toEqual(2147483647)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('n:mersenne-take-while(-> $ < 1000)')).toEqual([3, 7, 31, 127])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('n:mersenne?(0)')).toEqual(false)
    expect(lits.run('n:mersenne?(1)')).toEqual(false)
    expect(lits.run('n:mersenne?(2)')).toEqual(false)
    expect(lits.run('n:mersenne?(3)')).toEqual(true)
    expect(lits.run('n:mersenne?(4)')).toEqual(false)
    expect(lits.run('n:mersenne?(5)')).toEqual(false)
    expect(lits.run('n:mersenne?(6)')).toEqual(false)
    expect(lits.run('n:mersenne?(7)')).toEqual(true)
    expect(lits.run('n:mersenne?(8)')).toEqual(false)
    expect(lits.run('n:mersenne?(9)')).toEqual(false)
    expect(lits.run('n:mersenne?(2147483647)')).toEqual(true)
    expect(lits.run('n:mersenne?(2147483648)')).toEqual(false)
  })
})
