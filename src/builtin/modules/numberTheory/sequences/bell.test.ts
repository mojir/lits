import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { numberTheoryModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [numberTheoryModule] })

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('bell', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:bell-seq(1)')).toEqual([1])
    expect(runNth('nth:bell-seq(2)')).toEqual([1, 2])
    expect(runNth('nth:bell-seq(3)')).toEqual([1, 2, 5])
    expect(runNth('nth:bell-seq(4)')).toEqual([1, 2, 5, 15])
    expect(runNth('nth:bell-seq(22)')).toEqual([
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
    expect(() => runNth('nth:bell-seq(0)')).toThrow(LitsError)
    expect(() => runNth('nth:bell-seq(23)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:bell-nth(1)')).toEqual(1)
    expect(runNth('nth:bell-nth(2)')).toEqual(2)
    expect(runNth('nth:bell-nth(3)')).toEqual(5)
    expect(runNth('nth:bell-nth(22)')).toEqual(4506715738447323)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:bell-take-while(-> $ < 1000)')).toEqual([1, 2, 5, 15, 52, 203, 877])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:bell?(0)')).toEqual(false)
    expect(runNth('nth:bell?(1)')).toEqual(true)
    expect(runNth('nth:bell?(2)')).toEqual(true)
    expect(runNth('nth:bell?(3)')).toEqual(false)
    expect(runNth('nth:bell?(4)')).toEqual(false)
    expect(runNth('nth:bell?(5)')).toEqual(true)
    expect(runNth('nth:bell?(6)')).toEqual(false)
    expect(runNth('nth:bell?(7)')).toEqual(false)
    expect(runNth('nth:bell?(8)')).toEqual(false)
    expect(runNth('nth:bell?(9)')).toEqual(false)
  })
})
