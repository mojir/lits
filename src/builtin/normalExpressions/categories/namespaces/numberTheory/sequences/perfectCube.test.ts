import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'
import { LitsError } from '../../../../../../errors'

const lits = new Lits()
describe('perfect-cube', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:perfect-cube-seq(1)')).toEqual([1])
    expect(lits.run('nth:perfect-cube-seq(2)')).toEqual([1, 8])
    expect(lits.run('nth:perfect-cube-seq(3)')).toEqual([1, 8, 27])
    expect(lits.run('nth:perfect-cube-seq(4)')).toEqual([1, 8, 27, 64])
    expect(() => lits.run('nth:perfect-cube-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:perfect-cube-nth(1)')).toEqual(1)
    expect(lits.run('nth:perfect-cube-nth(2)')).toEqual(8)
    expect(lits.run('nth:perfect-cube-nth(3)')).toEqual(27)
    expect(lits.run('nth:perfect-cube-nth(4)')).toEqual(64)
    expect(lits.run('nth:perfect-cube-nth(5)')).toEqual(125)
    expect(lits.run('nth:perfect-cube-nth(100)')).toEqual(1000000)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:perfect-cube-take-while(-> $ < 100)')).toEqual([
      1,
      8,
      27,
      64,
    ])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:perfect-cube?(0)')).toEqual(false)
    expect(lits.run('nth:perfect-cube?(1)')).toEqual(true)
    expect(lits.run('nth:perfect-cube?(2)')).toEqual(false)
    expect(lits.run('nth:perfect-cube?(3)')).toEqual(false)
    expect(lits.run('nth:perfect-cube?(4)')).toEqual(false)
    expect(lits.run('nth:perfect-cube?(5)')).toEqual(false)
    expect(lits.run('nth:perfect-cube?(6)')).toEqual(false)
    expect(lits.run('nth:perfect-cube?(7)')).toEqual(false)
    expect(lits.run('nth:perfect-cube?(8)')).toEqual(true)
    expect(lits.run('nth:perfect-cube?(9)')).toEqual(false)
    expect(lits.run('nth:perfect-cube?(1000)')).toEqual(true)
    expect(lits.run('nth:perfect-cube?(10000)')).toEqual(false)
  })
})
