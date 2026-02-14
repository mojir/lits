import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { LitsError } from '../../../../errors'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('deficient', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:deficient-seq(1)')).toEqual([1])
    expect(runNth('nth:deficient-seq(2)')).toEqual([1, 2])
    expect(runNth('nth:deficient-seq(3)')).toEqual([1, 2, 3])
    expect(runNth('nth:deficient-seq(18)')).toEqual([1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 19, 21, 22])
    expect(() => runNth('nth:deficient-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:deficient-nth(1)')).toEqual(1)
    expect(runNth('nth:deficient-nth(2)')).toEqual(2)
    expect(runNth('nth:deficient-nth(3)')).toEqual(3)
    expect(runNth('nth:deficient-nth(4)')).toEqual(4)
    expect(runNth('nth:deficient-nth(20)')).toEqual(25)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:deficient-take-while(-> $ < 20)')).toEqual([
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
    expect(runNth('nth:deficient?(0)')).toEqual(false)
    expect(runNth('nth:deficient?(1)')).toEqual(true)
    expect(runNth('nth:deficient?(2)')).toEqual(true)
    expect(runNth('nth:deficient?(3)')).toEqual(true)
    expect(runNth('nth:deficient?(12)')).toEqual(false)
    expect(runNth('nth:deficient?(15)')).toEqual(true)
    expect(runNth('nth:deficient?(18)')).toEqual(false)
  })
})
