import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('sylvester', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('c:sylvester-seq(1)')).toEqual([2])
    expect(lits.run('c:sylvester-seq(2)')).toEqual([2, 6])
    expect(lits.run('c:sylvester-seq(3)')).toEqual([2, 6, 42])
    expect(lits.run('c:sylvester-seq(4)')).toEqual([2, 6, 42, 1806])
    expect(lits.run('c:sylvester-seq(5)')).toEqual([2, 6, 42, 1806, 3263442])
    expect(lits.run('c:sylvester-seq(6)')).toEqual([2, 6, 42, 1806, 3263442, 10650056950806])
    expect(() => lits.run('c:sylvester-seq(0)')).toThrow()
    expect(() => lits.run('c:sylvester-seq(7)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('c:sylvester-nth(1)')).toEqual(2)
    expect(lits.run('c:sylvester-nth(2)')).toEqual(6)
    expect(lits.run('c:sylvester-nth(3)')).toEqual(42)
    expect(lits.run('c:sylvester-nth(4)')).toEqual(1806)
    expect(lits.run('c:sylvester-nth(5)')).toEqual(3263442)
    expect(lits.run('c:sylvester-nth(6)')).toEqual(10650056950806)
    expect(() => lits.run('c:sylvester-nth(0)')).toThrow()
    expect(() => lits.run('c:sylvester-nth(7)')).toThrow()
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('c:sylvester-take-while(-> $ < 1000)')).toEqual([2, 6, 42])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('c:sylvester?(0)')).toEqual(false)
    expect(lits.run('c:sylvester?(1)')).toEqual(false)
    expect(lits.run('c:sylvester?(2)')).toEqual(true)
    expect(lits.run('c:sylvester?(3)')).toEqual(false)
    expect(lits.run('c:sylvester?(4)')).toEqual(false)
    expect(lits.run('c:sylvester?(5)')).toEqual(false)
    expect(lits.run('c:sylvester?(6)')).toEqual(true)
    expect(lits.run('c:sylvester?(7)')).toEqual(false)
    expect(lits.run('c:sylvester?(8)')).toEqual(false)
    expect(lits.run('c:sylvester?(9)')).toEqual(false)
    expect(lits.run('c:sylvester?(10650056950806)')).toEqual(true)
  })
})
