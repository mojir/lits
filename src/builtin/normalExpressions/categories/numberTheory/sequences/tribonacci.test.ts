import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('tribonacci', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('n:tribonacci-seq(1)')).toEqual([0])
    expect(lits.run('n:tribonacci-seq(2)')).toEqual([0, 1])
    expect(lits.run('n:tribonacci-seq(3)')).toEqual([0, 1, 1])
    expect(lits.run('n:tribonacci-seq(4)')).toEqual([0, 1, 1, 2])
    expect(lits.run('n:tribonacci-seq(11)')).toEqual([
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
    expect(() => lits.run('n:tribonacci-seq(0)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('n:tribonacci-nth(1)')).toEqual(0)
    expect(lits.run('n:tribonacci-nth(2)')).toEqual(1)
    expect(lits.run('n:tribonacci-nth(3)')).toEqual(1)
    expect(lits.run('n:tribonacci-nth(4)')).toEqual(2)
    expect(lits.run('n:tribonacci-nth(11)')).toEqual(149)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('n:tribonacci-take-while(-> $ < 100)')).toEqual([0, 1, 1, 2, 4, 7, 13, 24, 44, 81])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('n:tribonacci?(0)')).toEqual(true)
    expect(lits.run('n:tribonacci?(1)')).toEqual(true)
    expect(lits.run('n:tribonacci?(2)')).toEqual(true)
    expect(lits.run('n:tribonacci?(3)')).toEqual(false)
    expect(lits.run('n:tribonacci?(4)')).toEqual(true)
    expect(lits.run('n:tribonacci?(5)')).toEqual(false)
    expect(lits.run('n:tribonacci?(6)')).toEqual(false)
    expect(lits.run('n:tribonacci?(7)')).toEqual(true)
    expect(lits.run('n:tribonacci?(8)')).toEqual(false)
    expect(lits.run('n:tribonacci?(9)')).toEqual(false)
  })
})
