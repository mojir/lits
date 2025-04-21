import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('tribonacci', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:tribonacci-seq(1)')).toEqual([0])
    expect(lits.run('nth:tribonacci-seq(2)')).toEqual([0, 1])
    expect(lits.run('nth:tribonacci-seq(3)')).toEqual([0, 1, 1])
    expect(lits.run('nth:tribonacci-seq(4)')).toEqual([0, 1, 1, 2])
    expect(lits.run('nth:tribonacci-seq(11)')).toEqual([
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
    expect(() => lits.run('nth:tribonacci-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:tribonacci-nth(1)')).toEqual(0)
    expect(lits.run('nth:tribonacci-nth(2)')).toEqual(1)
    expect(lits.run('nth:tribonacci-nth(3)')).toEqual(1)
    expect(lits.run('nth:tribonacci-nth(4)')).toEqual(2)
    expect(lits.run('nth:tribonacci-nth(11)')).toEqual(149)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:tribonacci-take-while(-> $ < 100)')).toEqual([0, 1, 1, 2, 4, 7, 13, 24, 44, 81])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:tribonacci?(0)')).toEqual(true)
    expect(lits.run('nth:tribonacci?(1)')).toEqual(true)
    expect(lits.run('nth:tribonacci?(2)')).toEqual(true)
    expect(lits.run('nth:tribonacci?(3)')).toEqual(false)
    expect(lits.run('nth:tribonacci?(4)')).toEqual(true)
    expect(lits.run('nth:tribonacci?(5)')).toEqual(false)
    expect(lits.run('nth:tribonacci?(6)')).toEqual(false)
    expect(lits.run('nth:tribonacci?(7)')).toEqual(true)
    expect(lits.run('nth:tribonacci?(8)')).toEqual(false)
    expect(lits.run('nth:tribonacci?(9)')).toEqual(false)
  })
})
