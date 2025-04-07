import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('perfect', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('n:perfect-seq(1)')).toEqual([6])
    expect(lits.run('n:perfect-seq(2)')).toEqual([6, 28])
    expect(lits.run('n:perfect-seq(3)')).toEqual([6, 28, 496])
    expect(lits.run('n:perfect-seq(4)')).toEqual([6, 28, 496, 8128])
    expect(lits.run('n:perfect-seq(7)')).toEqual([6, 28, 496, 8128, 33550336, 8589869056, 137438691328])
    expect(lits.run('n:perfect-seq()')).toEqual([6, 28, 496, 8128, 33550336, 8589869056, 137438691328])
    expect(() => lits.run('n:perfect-seq(0)')).toThrow()
    expect(() => lits.run('n:perfect-seq(20)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('n:perfect-nth(1)')).toEqual(6)
    expect(lits.run('n:perfect-nth(2)')).toEqual(28)
    expect(lits.run('n:perfect-nth(3)')).toEqual(496)
    expect(lits.run('n:perfect-nth(4)')).toEqual(8128)
    expect(lits.run('n:perfect-nth(7)')).toEqual(137438691328)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('n:perfect-take-while(-> $ < 1000)')).toEqual([6, 28, 496])
    expect(lits.run('n:perfect-take-while(-> true)')).toEqual([6, 28, 496, 8128, 33550336, 8589869056, 137438691328])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('n:perfect?(0)')).toEqual(false)
    expect(lits.run('n:perfect?(1)')).toEqual(false)
    expect(lits.run('n:perfect?(2)')).toEqual(false)
    expect(lits.run('n:perfect?(3)')).toEqual(false)
    expect(lits.run('n:perfect?(4)')).toEqual(false)
    expect(lits.run('n:perfect?(5)')).toEqual(false)
    expect(lits.run('n:perfect?(6)')).toEqual(true)
    expect(lits.run('n:perfect?(7)')).toEqual(false)
    expect(lits.run('n:perfect?(8)')).toEqual(false)
    expect(lits.run('n:perfect?(9)')).toEqual(false)
    expect(lits.run('n:perfect?(137438691328)')).toEqual(true)
    expect(lits.run('n:perfect?(137438691329)')).toEqual(false)
  })
})
