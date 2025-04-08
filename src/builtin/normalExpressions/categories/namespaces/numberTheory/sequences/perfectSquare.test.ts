import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()
describe('perfect-square', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('n:perfect-square-seq(1)')).toEqual([1])
    expect(lits.run('n:perfect-square-seq(2)')).toEqual([1, 4])
    expect(lits.run('n:perfect-square-seq(3)')).toEqual([1, 4, 9])
    expect(lits.run('n:perfect-square-seq(4)')).toEqual([1, 4, 9, 16])
    expect(() => lits.run('n:perfect-square-seq(0)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('n:perfect-square-nth(1)')).toEqual(1)
    expect(lits.run('n:perfect-square-nth(2)')).toEqual(4)
    expect(lits.run('n:perfect-square-nth(3)')).toEqual(9)
    expect(lits.run('n:perfect-square-nth(4)')).toEqual(16)
    expect(lits.run('n:perfect-square-nth(5)')).toEqual(25)
    expect(lits.run('n:perfect-square-nth(100)')).toEqual(10000)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('n:perfect-square-take-while(-> $ < 100)')).toEqual([
      1,
      4,
      9,
      16,
      25,
      36,
      49,
      64,
      81,
    ])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('n:perfect-square?(0)')).toEqual(false)
    expect(lits.run('n:perfect-square?(1)')).toEqual(true)
    expect(lits.run('n:perfect-square?(2)')).toEqual(false)
    expect(lits.run('n:perfect-square?(3)')).toEqual(false)
    expect(lits.run('n:perfect-square?(4)')).toEqual(true)
    expect(lits.run('n:perfect-square?(5)')).toEqual(false)
    expect(lits.run('n:perfect-square?(6)')).toEqual(false)
    expect(lits.run('n:perfect-square?(7)')).toEqual(false)
    expect(lits.run('n:perfect-square?(8)')).toEqual(false)
    expect(lits.run('n:perfect-square?(9)')).toEqual(true)
    expect(lits.run('n:perfect-square?(100)')).toEqual(true)
    expect(lits.run('n:perfect-square?(1000)')).toEqual(false)
  })
})
