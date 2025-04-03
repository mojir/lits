import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('catalan', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('c:catalan-seq(1)')).toEqual([1])
    expect(lits.run('c:catalan-seq(2)')).toEqual([1, 2])
    expect(lits.run('c:catalan-seq(3)')).toEqual([1, 2, 5])
    expect(lits.run('c:catalan-seq(4)')).toEqual([1, 2, 5, 14])
    expect(lits.run('c:catalan-seq(30)')).toEqual([
      1,
      2,
      5,
      14,
      42,
      132,
      429,
      1430,
      4862,
      16796,
      58786,
      208012,
      742900,
      2674440,
      9694845,
      35357670,
      129644790,
      477638700,
      1767263190,
      6564120420,
      24466267020,
      91482563640,
      343059613650,
      1289904147324,
      4861946401452,
      18367353072152,
      69533550916004,
      263747951750360,
      1002242216651368,
      3814986502092304,
    ])
    expect(() => lits.run('c:catalan-seq(0)')).toThrow()
    expect(() => lits.run('c:catalan-seq(32)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('c:catalan-nth(1)')).toEqual(1)
    expect(lits.run('c:catalan-nth(2)')).toEqual(2)
    expect(lits.run('c:catalan-nth(3)')).toEqual(5)
    expect(lits.run('c:catalan-nth(4)')).toEqual(14)
    expect(lits.run('c:catalan-nth(30)')).toEqual(3814986502092304)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('c:catalan-take-while(-> $ < 1000)')).toEqual([1, 2, 5, 14, 42, 132, 429])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('c:catalan?(0)')).toEqual(false)
    expect(lits.run('c:catalan?(1)')).toEqual(true)
    expect(lits.run('c:catalan?(2)')).toEqual(true)
    expect(lits.run('c:catalan?(3)')).toEqual(false)
    expect(lits.run('c:catalan?(4)')).toEqual(false)
    expect(lits.run('c:catalan?(5)')).toEqual(true)
    expect(lits.run('c:catalan?(6)')).toEqual(false)
    expect(lits.run('c:catalan?(7)')).toEqual(false)
    expect(lits.run('c:catalan?(8)')).toEqual(false)
    expect(lits.run('c:catalan?(9)')).toEqual(false)
    expect(lits.run('c:catalan?(3814986502092303)')).toEqual(false)
    expect(lits.run('c:catalan?(3814986502092304)')).toEqual(true)
  })
})
