import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()
describe('arithmetic', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:arithmetic-seq(1, 1, 10)')).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    expect(lits.run('nth:arithmetic-seq(2, 3, 5)')).toEqual([2, 5, 8, 11, 14])
    expect(lits.run('nth:arithmetic-seq(1, 2, 4)')).toEqual([1, 3, 5, 7])
    expect(lits.run('nth:arithmetic-seq(1, 1.5, 4)')).toEqual([1, 2.5, 4, 5.5])
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:arithmetic-nth(1, 1, 10)')).toEqual(10)
    expect(lits.run('nth:arithmetic-nth(2, 3, 5)')).toEqual(14)
    expect(lits.run('nth:arithmetic-nth(1, 2, 4)')).toEqual(7)
    expect(lits.run('nth:arithmetic-nth(1, 1.5, 4)')).toEqual(5.5)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:arithmetic-take-while(1, 0.25, -> $ < 3)')).toEqual([1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:arithmetic?(1, 1, 10)')).toEqual(true)
    expect(lits.run('nth:arithmetic?(2, 3, 5)')).toEqual(true)
    expect(lits.run('nth:arithmetic?(1, 2, 4)')).toEqual(false)
    expect(lits.run('nth:arithmetic?(1, 1.5, 4)')).toEqual(true)
    expect(lits.run('nth:arithmetic?(1, 0, 1)')).toEqual(true)
    expect(lits.run('nth:arithmetic?(1, 0, 2)')).toEqual(false)
    expect(lits.run('nth:arithmetic?(1, 1, 0)')).toEqual(false)
    expect(lits.run('nth:arithmetic?(1, 0.000000001, 1.1)')).toEqual(true)
    expect(lits.run('nth:arithmetic?(1e-11, 0.01, 1e-11)')).toEqual(true)
  })
})
