import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { LitsError } from '../../../../errors'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('perfect-power', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:perfect-power-seq(1)')).toEqual([1])
    expect(runNth('nth:perfect-power-seq(2)')).toEqual([1, 4])
    expect(runNth('nth:perfect-power-seq(3)')).toEqual([1, 4, 8])
    expect(runNth('nth:perfect-power-seq(10)')).toEqual([1, 4, 8, 9, 16, 25, 27, 32, 36, 49])
    expect(() => runNth('nth:perfect-power-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:perfect-power-nth(1)')).toEqual(1)
    expect(runNth('nth:perfect-power-nth(2)')).toEqual(4)
    expect(runNth('nth:perfect-power-nth(3)')).toEqual(8)
    expect(runNth('nth:perfect-power-nth(4)')).toEqual(9)
    expect(runNth('nth:perfect-power-nth(5)')).toEqual(16)
    expect(runNth('nth:perfect-power-nth(6)')).toEqual(25)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:perfect-power-take-while(-> $ <= 100)')).toEqual([
      1,
      4,
      8,
      9,
      16,
      25,
      27,
      32,
      36,
      49,
      64,
      81,
      100,
    ])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:perfect-power?(0)')).toEqual(false)
    expect(runNth('nth:perfect-power?(1)')).toEqual(true)
    expect(runNth('nth:perfect-power?(2)')).toEqual(false)
    expect(runNth('nth:perfect-power?(3)')).toEqual(false)
    expect(runNth('nth:perfect-power?(4)')).toEqual(true)
    expect(runNth('nth:perfect-power?(5)')).toEqual(false)
    expect(runNth('nth:perfect-power?(6)')).toEqual(false)
    expect(runNth('nth:perfect-power?(7)')).toEqual(false)
    expect(runNth('nth:perfect-power?(8)')).toEqual(true)
    expect(runNth('nth:perfect-power?(9)')).toEqual(true)
    expect(runNth('nth:perfect-power?(100)')).toEqual(true)
  })

  it('should return tuple with base and exponent', () => {
    expect(runNth('nth:perfect-power(1)')).toEqual([1, 2])
    expect(runNth('nth:perfect-power(4)')).toEqual([2, 2])
    expect(runNth('nth:perfect-power(8)')).toEqual([2, 3])
    expect(runNth('nth:perfect-power(9)')).toEqual([3, 2])
    expect(runNth('nth:perfect-power(16)')).toEqual([4, 2])
    expect(runNth('nth:perfect-power(25)')).toEqual([5, 2])
    expect(runNth('nth:perfect-power(27)')).toEqual([3, 3])
    expect(runNth('nth:perfect-power(32)')).toEqual([2, 5])
    expect(runNth('nth:perfect-power(36)')).toEqual([6, 2])
    expect(runNth('nth:perfect-power(49)')).toEqual([7, 2])
    expect(runNth('nth:perfect-power(64)')).toEqual([8, 2])
    expect(runNth('nth:perfect-power(81)')).toEqual([9, 2])
    expect(runNth('nth:perfect-power(99)')).toEqual(null)
  })
})
