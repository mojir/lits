import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { numberTheoryModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [numberTheoryModule] })

function runNth(code: string) {
  return lits.run(`let nt = import(number-theory); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('mersenne', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:mersenne-seq(1)')).toEqual([3])
    expect(runNth('nth:mersenne-seq(2)')).toEqual([3, 7])
    expect(runNth('nth:mersenne-seq(3)')).toEqual([3, 7, 31])
    expect(runNth('nth:mersenne-seq(4)')).toEqual([3, 7, 31, 127])
    expect(runNth('nth:mersenne-seq(9)')).toEqual([3, 7, 31, 127, 2047, 8191, 131071, 524287, 2147483647])
    expect(() => runNth('nth:mersenne-seq(0)')).toThrow(LitsError)
    expect(() => runNth('nth:mersenne-seq(20)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:mersenne-nth(1)')).toEqual(3)
    expect(runNth('nth:mersenne-nth(2)')).toEqual(7)
    expect(runNth('nth:mersenne-nth(3)')).toEqual(31)
    expect(runNth('nth:mersenne-nth(4)')).toEqual(127)
    expect(runNth('nth:mersenne-nth(9)')).toEqual(2147483647)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:mersenne-take-while(-> $ < 1000)')).toEqual([3, 7, 31, 127])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:mersenne?(0)')).toEqual(false)
    expect(runNth('nth:mersenne?(1)')).toEqual(false)
    expect(runNth('nth:mersenne?(2)')).toEqual(false)
    expect(runNth('nth:mersenne?(3)')).toEqual(true)
    expect(runNth('nth:mersenne?(4)')).toEqual(false)
    expect(runNth('nth:mersenne?(5)')).toEqual(false)
    expect(runNth('nth:mersenne?(6)')).toEqual(false)
    expect(runNth('nth:mersenne?(7)')).toEqual(true)
    expect(runNth('nth:mersenne?(8)')).toEqual(false)
    expect(runNth('nth:mersenne?(9)')).toEqual(false)
    expect(runNth('nth:mersenne?(2147483647)')).toEqual(true)
    expect(runNth('nth:mersenne?(2147483648)')).toEqual(false)
  })
})
