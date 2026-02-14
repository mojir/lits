import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { LitsError } from '../../../../errors'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('tribonacci', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:tribonacci-seq(1)')).toEqual([0])
    expect(runNth('nth:tribonacci-seq(2)')).toEqual([0, 1])
    expect(runNth('nth:tribonacci-seq(3)')).toEqual([0, 1, 1])
    expect(runNth('nth:tribonacci-seq(4)')).toEqual([0, 1, 1, 2])
    expect(runNth('nth:tribonacci-seq(11)')).toEqual([
      0,
      1,
      1,
      2,
      4,
      7,
      13,
      24,
      44,
      81,
      149,
    ])
    expect(() => runNth('nth:tribonacci-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:tribonacci-nth(1)')).toEqual(0)
    expect(runNth('nth:tribonacci-nth(2)')).toEqual(1)
    expect(runNth('nth:tribonacci-nth(3)')).toEqual(1)
    expect(runNth('nth:tribonacci-nth(4)')).toEqual(2)
    expect(runNth('nth:tribonacci-nth(11)')).toEqual(149)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:tribonacci-take-while(-> $ < 100)')).toEqual([0, 1, 1, 2, 4, 7, 13, 24, 44, 81])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:tribonacci?(0)')).toEqual(true)
    expect(runNth('nth:tribonacci?(1)')).toEqual(true)
    expect(runNth('nth:tribonacci?(2)')).toEqual(true)
    expect(runNth('nth:tribonacci?(3)')).toEqual(false)
    expect(runNth('nth:tribonacci?(4)')).toEqual(true)
    expect(runNth('nth:tribonacci?(5)')).toEqual(false)
    expect(runNth('nth:tribonacci?(6)')).toEqual(false)
    expect(runNth('nth:tribonacci?(7)')).toEqual(true)
    expect(runNth('nth:tribonacci?(8)')).toEqual(false)
    expect(runNth('nth:tribonacci?(9)')).toEqual(false)
  })
})
