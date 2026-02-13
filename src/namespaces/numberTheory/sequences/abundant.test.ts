import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { LitsError } from '../../../errors'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('abundant', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:abundant-seq(1)')).toEqual([12])
    expect(runNth('nth:abundant-seq(2)')).toEqual([12, 18])
    expect(runNth('nth:abundant-seq(3)')).toEqual([12, 18, 20])
    expect(runNth('nth:abundant-seq(21)')).toEqual([12, 18, 20, 24, 30, 36, 40, 42, 48, 54, 56, 60, 66, 70, 72, 78, 80, 84, 88, 90, 96])
    expect(() => runNth('nth:abundant-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:abundant-nth(1)')).toEqual(12)
    expect(runNth('nth:abundant-nth(2)')).toEqual(18)
    expect(runNth('nth:abundant-nth(3)')).toEqual(20)
    expect(runNth('nth:abundant-nth(4)')).toEqual(24)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:abundant-take-while(-> $ < 20)')).toEqual([
      12,
      18,
    ])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:abundant?(0)')).toEqual(false)
    expect(runNth('nth:abundant?(1)')).toEqual(false)
    expect(runNth('nth:abundant?(2)')).toEqual(false)
    expect(runNth('nth:abundant?(3)')).toEqual(false)
    expect(runNth('nth:abundant?(12)')).toEqual(true)
    expect(runNth('nth:abundant?(15)')).toEqual(false)
    expect(runNth('nth:abundant?(18)')).toEqual(true)
  })
})
