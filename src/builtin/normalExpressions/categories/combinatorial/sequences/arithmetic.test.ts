import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('arithmetic', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('c:arithmetic-seq(1, 1, 10)')).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    expect(lits.run('c:arithmetic-seq(2, 3, 5)')).toEqual([2, 5, 8, 11, 14])
    expect(lits.run('c:arithmetic-seq(1, 2, 4)')).toEqual([1, 3, 5, 7])
    expect(lits.run('c:arithmetic-seq(1, 1.5, 4)')).toEqual([1, 2.5, 4, 5.5])
  })

  it('should return the correct nth term', () => {
    expect(lits.run('c:arithmetic-nth(1, 1, 10)')).toEqual(10)
    expect(lits.run('c:arithmetic-nth(2, 3, 5)')).toEqual(14)
    expect(lits.run('c:arithmetic-nth(1, 2, 4)')).toEqual(7)
    expect(lits.run('c:arithmetic-nth(1, 1.5, 4)')).toEqual(5.5)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('c:arithmetic-take-while(1, 0.25, -> $ < 3)')).toEqual([1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('c:arithmetic?(1, 1, 10)')).toEqual(true)
    expect(lits.run('c:arithmetic?(2, 3, 5)')).toEqual(true)
    expect(lits.run('c:arithmetic?(1, 2, 4)')).toEqual(false)
    expect(lits.run('c:arithmetic?(1, 1.5, 4)')).toEqual(true)
  })
})
