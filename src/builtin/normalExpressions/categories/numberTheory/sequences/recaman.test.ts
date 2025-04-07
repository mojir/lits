import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../Lits/Lits'

const lits = new Lits()
describe('recaman', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('n:recaman-seq(1)')).toEqual([0])
    expect(lits.run('n:recaman-seq(2)')).toEqual([0, 1])
    expect(lits.run('n:recaman-seq(3)')).toEqual([0, 1, 3])
    expect(lits.run('n:recaman-seq(4)')).toEqual([0, 1, 3, 6])
    expect(lits.run('n:recaman-seq(5)')).toEqual([0, 1, 3, 6, 2])
    expect(lits.run('n:recaman-seq(6)')).toEqual([0, 1, 3, 6, 2, 7])
    expect(lits.run('n:recaman-seq(71)')).toEqual([0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9, 24, 8, 25, 43, 62, 42, 63, 41, 18, 42, 17, 43, 16, 44, 15, 45, 14, 46, 79, 113, 78, 114, 77, 39, 78, 38, 79, 37, 80, 36, 81, 35, 82, 34, 83, 33, 84, 32, 85, 31, 86, 30, 87, 29, 88, 28, 89, 27, 90, 26, 91, 157, 224, 156, 225, 155])
  })

  it('should return the correct nth term', () => {
    expect(lits.run('n:recaman-nth(1)')).toEqual(0)
    expect(lits.run('n:recaman-nth(2)')).toEqual(1)
    expect(lits.run('n:recaman-nth(3)')).toEqual(3)
    expect(lits.run('n:recaman-nth(4)')).toEqual(6)
    expect(lits.run('n:recaman-nth(5)')).toEqual(2)
    expect(lits.run('n:recaman-nth(6)')).toEqual(7)
    expect(lits.run('n:recaman-nth(7)')).toEqual(13)
    expect(lits.run('n:recaman-nth(8)')).toEqual(20)
    expect(lits.run('n:recaman-nth(9)')).toEqual(12)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('n:recaman-take-while(-> $ < 8)')).toEqual([0, 1, 3, 6, 2, 7])
    expect(lits.run('n:recaman-take-while(-> $2 < 10)')).toEqual([0, 1, 3, 6, 2, 7, 13, 20, 12, 21])
    expect(lits.run('n:recaman-take-while(-> false)')).toEqual([])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('n:recaman?(0)')).toEqual(true)
    expect(lits.run('n:recaman?(1)')).toEqual(true)
    expect(lits.run('n:recaman?(2)')).toEqual(true)
    expect(lits.run('n:recaman?(3)')).toEqual(true)
    expect(lits.run('n:recaman?(4)')).toEqual(true)
    expect(lits.run('n:recaman?(5)')).toEqual(true)
    expect(lits.run('n:recaman?(6)')).toEqual(true)
    expect(lits.run('n:recaman?(7)')).toEqual(true)
    expect(lits.run('n:recaman?(8)')).toEqual(true)
    expect(lits.run('n:recaman?(9)')).toEqual(true)
  })
})
