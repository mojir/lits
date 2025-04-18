import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()
describe('padovan', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:padovan-seq(1)')).toEqual([1])
    expect(lits.run('nth:padovan-seq(2)')).toEqual([1, 1])
    expect(lits.run('nth:padovan-seq(3)')).toEqual([1, 1, 1])
    expect(lits.run('nth:padovan-seq(4)')).toEqual([1, 1, 1, 2])
    expect(lits.run('nth:padovan-seq(22)')).toEqual([1, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 16, 21, 28, 37, 49, 65, 86, 114, 151, 200, 265])
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:padovan-nth(1)')).toEqual(1)
    expect(lits.run('nth:padovan-nth(2)')).toEqual(1)
    expect(lits.run('nth:padovan-nth(3)')).toEqual(1)
    expect(lits.run('nth:padovan-nth(4)')).toEqual(2)
    expect(lits.run('nth:padovan-nth(5)')).toEqual(2)
    expect(lits.run('nth:padovan-nth(6)')).toEqual(3)
    expect(lits.run('nth:padovan-nth(7)')).toEqual(4)
    expect(lits.run('nth:padovan-nth(8)')).toEqual(5)
    expect(lits.run('nth:padovan-nth(22)')).toEqual(265)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:padovan-take-while(-> $ < 8)')).toEqual([1, 1, 1, 2, 2, 3, 4, 5, 7])
    expect(lits.run('nth:padovan-take-while(-> $2 < 10)')).toEqual([1, 1, 1, 2, 2, 3, 4, 5, 7])
    expect(lits.run('nth:padovan-take-while(-> $2 < 0)')).toEqual([])
    expect(lits.run('nth:padovan-take-while(-> $2 < 1)')).toEqual([1])
    expect(lits.run('nth:padovan-take-while(-> $2 < 2)')).toEqual([1, 1])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:padovan?(0)')).toEqual(false)
    expect(lits.run('nth:padovan?(1)')).toEqual(true)
    expect(lits.run('nth:padovan?(2)')).toEqual(true)
    expect(lits.run('nth:padovan?(3)')).toEqual(true)
    expect(lits.run('nth:padovan?(4)')).toEqual(true)
    expect(lits.run('nth:padovan?(5)')).toEqual(true)
    expect(lits.run('nth:padovan?(6)')).toEqual(false)
    expect(lits.run('nth:padovan?(7)')).toEqual(true)
    expect(lits.run('nth:padovan?(8)')).toEqual(false)
    expect(lits.run('nth:padovan?(265)')).toEqual(true)
    expect(lits.run('nth:padovan?(922111)')).toEqual(true)
    expect(lits.run(`nth:padovan?(${Number.MAX_SAFE_INTEGER - 1})`)).toEqual(false)
  })
})
