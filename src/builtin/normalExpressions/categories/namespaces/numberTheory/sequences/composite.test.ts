import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('composite', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:composite-seq(1)')).toEqual([4])
    expect(lits.run('nth:composite-seq(2)')).toEqual([4, 6])
    expect(lits.run('nth:composite-seq(3)')).toEqual([4, 6, 8])
    expect(lits.run('nth:composite-seq(4)')).toEqual([4, 6, 8, 9])
    expect(() => lits.run('nth:composite-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:composite-nth(1)')).toEqual(4)
    expect(lits.run('nth:composite-nth(2)')).toEqual(6)
    expect(lits.run('nth:composite-nth(3)')).toEqual(8)
    expect(lits.run('nth:composite-nth(4)')).toEqual(9)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:composite-take-while(-> $ < 20)')).toEqual([
      4,
      6,
      8,
      9,
      10,
      12,
      14,
      15,
      16,
      18,
    ])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:composite?(0)')).toEqual(false)
    expect(lits.run('nth:composite?(1)')).toEqual(false)
    expect(lits.run('nth:composite?(2)')).toEqual(false)
    expect(lits.run('nth:composite?(3)')).toEqual(false)
    expect(lits.run('nth:composite?(4)')).toEqual(true)
    expect(lits.run('nth:composite?(5)')).toEqual(false)
    expect(lits.run('nth:composite?(6)')).toEqual(true)
    expect(lits.run('nth:composite?(7)')).toEqual(false)
    expect(lits.run('nth:composite?(8)')).toEqual(true)
    expect(lits.run('nth:composite?(9)')).toEqual(true)
    expect(lits.run('nth:composite?(997)')).toEqual(false)
    expect(lits.run('nth:composite?(1001)')).toEqual(true)
  })
})
