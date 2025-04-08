import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()
describe('happy', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('n:happy-seq(1)')).toEqual([1])
    expect(lits.run('n:happy-seq(2)')).toEqual([1, 7])
    expect(lits.run('n:happy-seq(3)')).toEqual([1, 7, 10])
    expect(lits.run('n:happy-seq(4)')).toEqual([1, 7, 10, 13])
    expect(lits.run('n:happy-seq(20)')).toEqual([1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97, 100])
    expect(() => lits.run('n:happy-seq(0)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('n:happy-nth(1)')).toEqual(1)
    expect(lits.run('n:happy-nth(2)')).toEqual(7)
    expect(lits.run('n:happy-nth(3)')).toEqual(10)
    expect(lits.run('n:happy-nth(4)')).toEqual(13)
    expect(lits.run('n:happy-nth(5)')).toEqual(19)
    expect(lits.run('n:happy-nth(6)')).toEqual(23)
    expect(lits.run('n:happy-nth(7)')).toEqual(28)
    expect(lits.run('n:happy-nth(8)')).toEqual(31)
    expect(lits.run('n:happy-nth(20)')).toEqual(100)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('n:happy-take-while(-> $ < 100)')).toEqual([1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('n:happy?(0)')).toEqual(false)
    expect(lits.run('n:happy?(1)')).toEqual(true)
    expect(lits.run('n:happy?(2)')).toEqual(false)
    expect(lits.run('n:happy?(3)')).toEqual(false)
    expect(lits.run('n:happy?(4)')).toEqual(false)
    expect(lits.run('n:happy?(5)')).toEqual(false)
    expect(lits.run('n:happy?(6)')).toEqual(false)
    expect(lits.run('n:happy?(7)')).toEqual(true)
    expect(lits.run('n:happy?(8)')).toEqual(false)
    expect(lits.run('n:happy?(100)')).toEqual(true)
    expect(lits.run('n:happy?(101)')).toEqual(false)
  })
})
