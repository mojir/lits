import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('abundant', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:abundant-seq(1)')).toEqual([12])
    expect(lits.run('nth:abundant-seq(2)')).toEqual([12, 18])
    expect(lits.run('nth:abundant-seq(3)')).toEqual([12, 18, 20])
    expect(lits.run('nth:abundant-seq(21)')).toEqual([12, 18, 20, 24, 30, 36, 40, 42, 48, 54, 56, 60, 66, 70, 72, 78, 80, 84, 88, 90, 96])
    expect(() => lits.run('nth:abundant-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:abundant-nth(1)')).toEqual(12)
    expect(lits.run('nth:abundant-nth(2)')).toEqual(18)
    expect(lits.run('nth:abundant-nth(3)')).toEqual(20)
    expect(lits.run('nth:abundant-nth(4)')).toEqual(24)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:abundant-take-while(-> $ < 20)')).toEqual([
      12,
      18,
    ])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:abundant?(0)')).toEqual(false)
    expect(lits.run('nth:abundant?(1)')).toEqual(false)
    expect(lits.run('nth:abundant?(2)')).toEqual(false)
    expect(lits.run('nth:abundant?(3)')).toEqual(false)
    expect(lits.run('nth:abundant?(12)')).toEqual(true)
    expect(lits.run('nth:abundant?(15)')).toEqual(false)
    expect(lits.run('nth:abundant?(18)')).toEqual(true)
  })
})
