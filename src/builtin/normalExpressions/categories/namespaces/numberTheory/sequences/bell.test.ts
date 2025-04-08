import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../../../Lits/Lits'

const lits = new Lits()
describe('bell', () => {
  it('should return the correct sequence', () => {
    expect(lits.run('nth:bell-seq(1)')).toEqual([1])
    expect(lits.run('nth:bell-seq(2)')).toEqual([1, 2])
    expect(lits.run('nth:bell-seq(3)')).toEqual([1, 2, 5])
    expect(lits.run('nth:bell-seq(4)')).toEqual([1, 2, 5, 15])
    expect(lits.run('nth:bell-seq(22)')).toEqual([
      1,
      2,
      5,
      15,
      52,
      203,
      877,
      4140,
      21147,
      115975,
      678570,
      4213597,
      27644437,
      190899322,
      1382958545,
      10480142147,
      82864869804,
      682076806159,
      5832742205057,
      51724158235372,
      474869816156751,
      4506715738447323,
    ])
    expect(() => lits.run('nth:bell-seq(0)')).toThrow()
    expect(() => lits.run('nth:bell-seq(23)')).toThrow()
  })

  it('should return the correct nth term', () => {
    expect(lits.run('nth:bell-nth(1)')).toEqual(1)
    expect(lits.run('nth:bell-nth(2)')).toEqual(2)
    expect(lits.run('nth:bell-nth(3)')).toEqual(5)
    expect(lits.run('nth:bell-nth(22)')).toEqual(4506715738447323)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(lits.run('nth:bell-take-while(-> $ < 1000)')).toEqual([1, 2, 5, 15, 52, 203, 877])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(lits.run('nth:bell?(0)')).toEqual(false)
    expect(lits.run('nth:bell?(1)')).toEqual(true)
    expect(lits.run('nth:bell?(2)')).toEqual(true)
    expect(lits.run('nth:bell?(3)')).toEqual(false)
    expect(lits.run('nth:bell?(4)')).toEqual(false)
    expect(lits.run('nth:bell?(5)')).toEqual(true)
    expect(lits.run('nth:bell?(6)')).toEqual(false)
    expect(lits.run('nth:bell?(7)')).toEqual(false)
    expect(lits.run('nth:bell?(8)')).toEqual(false)
    expect(lits.run('nth:bell?(9)')).toEqual(false)
  })
})
