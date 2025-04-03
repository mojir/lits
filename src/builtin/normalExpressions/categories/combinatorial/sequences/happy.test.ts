import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('happy', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('c:happy-seq(1)')).toEqual([1])
    expect(lits.run('c:happy-seq(2)')).toEqual([1, 7])
    expect(lits.run('c:happy-seq(3)')).toEqual([1, 7, 10])
    expect(lits.run('c:happy-seq(4)')).toEqual([1, 7, 10, 13])
    expect(lits.run('c:happy-seq(20)')).toEqual([1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97, 100])
    expect(() => lits.run('c:happy-seq(0)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('c:happy-nth(1)')).toEqual(1)
    expect(lits.run('c:happy-nth(2)')).toEqual(7)
    expect(lits.run('c:happy-nth(3)')).toEqual(10)
    expect(lits.run('c:happy-nth(4)')).toEqual(13)
    expect(lits.run('c:happy-nth(5)')).toEqual(19)
    expect(lits.run('c:happy-nth(6)')).toEqual(23)
    expect(lits.run('c:happy-nth(7)')).toEqual(28)
    expect(lits.run('c:happy-nth(8)')).toEqual(31)
    expect(lits.run('c:happy-nth(20)')).toEqual(100)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('c:happy-take-while(-> $ < 100)')).toEqual([1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('c:happy?(0)')).toEqual(false)
    expect(lits.run('c:happy?(1)')).toEqual(true)
    expect(lits.run('c:happy?(2)')).toEqual(false)
    expect(lits.run('c:happy?(3)')).toEqual(false)
    expect(lits.run('c:happy?(4)')).toEqual(false)
    expect(lits.run('c:happy?(5)')).toEqual(false)
    expect(lits.run('c:happy?(6)')).toEqual(false)
    expect(lits.run('c:happy?(7)')).toEqual(true)
    expect(lits.run('c:happy?(8)')).toEqual(false)
    expect(lits.run('c:happy?(100)')).toEqual(true)
    expect(lits.run('c:happy?(101)')).toEqual(false)
  })
})
