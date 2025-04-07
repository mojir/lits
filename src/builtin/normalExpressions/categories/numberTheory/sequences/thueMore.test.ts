import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('thueMore', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('n:thue-morse-seq(1)')).toEqual([0])
    expect(lits.run('n:thue-morse-seq(2)')).toEqual([0, 1])
    expect(lits.run('n:thue-morse-seq(3)')).toEqual([0, 1, 1])
    expect(lits.run('n:thue-morse-seq(4)')).toEqual([0, 1, 1, 0])
    expect(lits.run('n:thue-morse-seq(5)')).toEqual([0, 1, 1, 0, 1])
  })

  it('should return the correct nth term', () => {
    expect(lits.run('n:thue-morse-nth(1)')).toEqual(0)
    expect(lits.run('n:thue-morse-nth(2)')).toEqual(1)
    expect(lits.run('n:thue-morse-nth(3)')).toEqual(1)
    expect(lits.run('n:thue-morse-nth(4)')).toEqual(0)
    expect(lits.run('n:thue-morse-nth(5)')).toEqual(1)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('n:thue-morse-take-while(-> $2 < 5)')).toEqual([0, 1, 1, 0, 1])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('n:thue-morse?(0)')).toEqual(true)
    expect(lits.run('n:thue-morse?(1)')).toEqual(true)
    expect(lits.run('n:thue-morse?(2)')).toEqual(false)
  })
})
