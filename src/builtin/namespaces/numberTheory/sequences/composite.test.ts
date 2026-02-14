import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { LitsError } from '../../../../errors'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('composite', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:composite-seq(1)')).toEqual([4])
    expect(runNth('nth:composite-seq(2)')).toEqual([4, 6])
    expect(runNth('nth:composite-seq(3)')).toEqual([4, 6, 8])
    expect(runNth('nth:composite-seq(4)')).toEqual([4, 6, 8, 9])
    expect(() => runNth('nth:composite-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:composite-nth(1)')).toEqual(4)
    expect(runNth('nth:composite-nth(2)')).toEqual(6)
    expect(runNth('nth:composite-nth(3)')).toEqual(8)
    expect(runNth('nth:composite-nth(4)')).toEqual(9)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:composite-take-while(-> $ < 20)')).toEqual([
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
    expect(runNth('nth:composite?(0)')).toEqual(false)
    expect(runNth('nth:composite?(1)')).toEqual(false)
    expect(runNth('nth:composite?(2)')).toEqual(false)
    expect(runNth('nth:composite?(3)')).toEqual(false)
    expect(runNth('nth:composite?(4)')).toEqual(true)
    expect(runNth('nth:composite?(5)')).toEqual(false)
    expect(runNth('nth:composite?(6)')).toEqual(true)
    expect(runNth('nth:composite?(7)')).toEqual(false)
    expect(runNth('nth:composite?(8)')).toEqual(true)
    expect(runNth('nth:composite?(9)')).toEqual(true)
    expect(runNth('nth:composite?(997)')).toEqual(false)
    expect(runNth('nth:composite?(1001)')).toEqual(true)
  })
})
