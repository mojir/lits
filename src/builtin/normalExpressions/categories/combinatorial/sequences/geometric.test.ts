import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('geometric', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('c:geometric-seq(3, 2, 2)')).toEqual([3, 6])
    expect(lits.run('c:geometric-seq(2, 3, 2)')).toEqual([2, 6])
    expect(lits.run('c:geometric-seq(1, 2, 2)')).toEqual([1, 4])
    expect(lits.run('c:geometric-seq(1, 1.5, 4)')).toEqual([1, 1.5, 2.25, 3.375])
  })

  it('should return the correct nth term', () => {
    expect(lits.run('c:geometric-nth(3, 2, 2)')).toEqual(12)
    expect(lits.run('c:geometric-nth(2, 3, 2)')).toEqual(18)
    expect(lits.run('c:geometric-nth(1, 2, 2)')).toEqual(4)
    expect(lits.run('c:geometric-nth(1, 1.5, 4)')).toEqual(5.0625)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('c:geometric-take-while(1, 1.5, -> $ < 10)')).toEqual([1, 1.5, 2.25, 3.375, 5.0625, 7.59375])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('c:geometric?(1, 2, 1)')).toEqual(true)
    expect(lits.run('c:geometric?(2, 3, 2)')).toEqual(true)
    expect(lits.run('c:geometric?(3, 2, 2)')).toEqual(false)
    expect(lits.run('c:geometric?(1, 1.5, 2.25)')).toEqual(true)
    expect(lits.run('c:geometric?(1, 1.5, -4)')).toEqual(false)
  })
})
