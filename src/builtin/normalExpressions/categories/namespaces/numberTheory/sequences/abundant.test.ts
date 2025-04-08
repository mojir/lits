import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()
describe('abundant', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('n:abundant-seq(1)')).toEqual([12])
    expect(lits.run('n:abundant-seq(2)')).toEqual([12, 18])
    expect(lits.run('n:abundant-seq(3)')).toEqual([12, 18, 20])
    expect(lits.run('n:abundant-seq(21)')).toEqual([12, 18, 20, 24, 30, 36, 40, 42, 48, 54, 56, 60, 66, 70, 72, 78, 80, 84, 88, 90, 96])
    expect(() => lits.run('n:abundant-seq(0)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('n:abundant-nth(1)')).toEqual(12)
    expect(lits.run('n:abundant-nth(2)')).toEqual(18)
    expect(lits.run('n:abundant-nth(3)')).toEqual(20)
    expect(lits.run('n:abundant-nth(4)')).toEqual(24)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('n:abundant-take-while(-> $ < 20)')).toEqual([
      12,
      18,
    ])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('n:abundant?(0)')).toEqual(false)
    expect(lits.run('n:abundant?(1)')).toEqual(false)
    expect(lits.run('n:abundant?(2)')).toEqual(false)
    expect(lits.run('n:abundant?(3)')).toEqual(false)
    expect(lits.run('n:abundant?(12)')).toEqual(true)
    expect(lits.run('n:abundant?(15)')).toEqual(false)
    expect(lits.run('n:abundant?(18)')).toEqual(true)
  })
})
