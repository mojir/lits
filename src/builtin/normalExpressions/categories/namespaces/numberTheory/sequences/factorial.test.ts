import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()
describe('factorial', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('n:factorial-seq(1)')).toEqual([1])
    expect(lits.run('n:factorial-seq(2)')).toEqual([1, 1])
    expect(lits.run('n:factorial-seq(3)')).toEqual([1, 1, 2])
    expect(lits.run('n:factorial-seq(4)')).toEqual([1, 1, 2, 6])
    expect(lits.run('n:factorial-seq(19)')).toEqual([
      1,
      1,
      2,
      6,
      24,
      120,
      720,
      5040,
      40320,
      362880,
      3628800,
      39916800,
      479001600,
      6227020800,
      87178291200,
      1307674368000,
      20922789888000,
      355687428096000,
      6402373705728000,
    ])
    expect(() => lits.run('n:factorial-seq(0)')).toThrow()
    expect(() => lits.run('n:factorial-seq(20)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('n:factorial-nth(1)')).toEqual(1)
    expect(lits.run('n:factorial-nth(2)')).toEqual(1)
    expect(lits.run('n:factorial-nth(3)')).toEqual(2)
    expect(lits.run('n:factorial-nth(4)')).toEqual(6)
    expect(lits.run('n:factorial-nth(19)')).toEqual(6402373705728000)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('n:factorial-take-while(-> $ < 1000)')).toEqual([1, 1, 2, 6, 24, 120, 720])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('n:factorial?(0)')).toEqual(false)
    expect(lits.run('n:factorial?(1)')).toEqual(true)
    expect(lits.run('n:factorial?(2)')).toEqual(true)
    expect(lits.run('n:factorial?(3)')).toEqual(false)
    expect(lits.run('n:factorial?(4)')).toEqual(false)
    expect(lits.run('n:factorial?(5)')).toEqual(false)
    expect(lits.run('n:factorial?(6)')).toEqual(true)
    expect(lits.run('n:factorial?(7)')).toEqual(false)
    expect(lits.run('n:factorial?(8)')).toEqual(false)
    expect(lits.run('n:factorial?(9)')).toEqual(false)
    expect(lits.run('n:factorial?(6402373705728000)')).toEqual(true)
    expect(lits.run('n:factorial?(6402373705728001)')).toEqual(false)
  })
})
