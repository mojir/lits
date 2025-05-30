import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('polygonal', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:polygonal-seq(3, 2)')).toEqual([1, 3])
    expect(lits.run('nth:polygonal-seq(4, 2)')).toEqual([1, 4])
    expect(lits.run('nth:polygonal-seq(5, 3)')).toEqual([1, 5, 12])
    expect(lits.run('nth:polygonal-seq(6, 5)')).toEqual([1, 6, 15, 28, 45])
    expect(() => lits.run('nth:polygonal-seq(2, 1)')).toThrow(LitsError)
    expect(() => lits.run('nth:polygonal-seq(3, 0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:polygonal-nth(3, 9)')).toEqual(45)
    expect(lits.run('nth:polygonal-nth(4, 5)')).toEqual(25)
    expect(lits.run('nth:polygonal-nth(5, 5)')).toEqual(35)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:polygonal-take-while(4, -> $ <= 100)')).toEqual([1, 4, 9, 16, 25, 36, 49, 64, 81, 100])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:polygonal?(3, 10)')).toEqual(true)
    expect(lits.run('nth:polygonal?(3, 9)')).toEqual(false)
    expect(lits.run('nth:polygonal?(5, 2)')).toEqual(false)
    expect(lits.run('nth:polygonal?(3, -9)')).toEqual(false)
    expect(lits.run('nth:polygonal?(4, 10000)')).toEqual(true)
    expect(lits.run('nth:polygonal?(4, 1000)')).toEqual(false)
    expect(lits.run('nth:polygonal?(6, 45)')).toEqual(true)
  })
})
