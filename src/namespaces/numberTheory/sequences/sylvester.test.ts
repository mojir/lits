import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { LitsError } from '../../../errors'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("TEMP-nth"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('sylvester', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:sylvester-seq(1)')).toEqual([2])
    expect(runNth('nth:sylvester-seq(2)')).toEqual([2, 6])
    expect(runNth('nth:sylvester-seq(3)')).toEqual([2, 6, 42])
    expect(runNth('nth:sylvester-seq(4)')).toEqual([2, 6, 42, 1806])
    expect(runNth('nth:sylvester-seq(5)')).toEqual([2, 6, 42, 1806, 3263442])
    expect(runNth('nth:sylvester-seq(6)')).toEqual([2, 6, 42, 1806, 3263442, 10650056950806])
    expect(() => runNth('nth:sylvester-seq(0)')).toThrow(LitsError)
    expect(() => runNth('nth:sylvester-seq(7)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:sylvester-nth(1)')).toEqual(2)
    expect(runNth('nth:sylvester-nth(2)')).toEqual(6)
    expect(runNth('nth:sylvester-nth(3)')).toEqual(42)
    expect(runNth('nth:sylvester-nth(4)')).toEqual(1806)
    expect(runNth('nth:sylvester-nth(5)')).toEqual(3263442)
    expect(runNth('nth:sylvester-nth(6)')).toEqual(10650056950806)
    expect(() => runNth('nth:sylvester-nth(0)')).toThrow(LitsError)
    expect(() => runNth('nth:sylvester-nth(7)')).toThrow(LitsError)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:sylvester-take-while(-> $ < 1000)')).toEqual([2, 6, 42])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:sylvester?(0)')).toEqual(false)
    expect(runNth('nth:sylvester?(1)')).toEqual(false)
    expect(runNth('nth:sylvester?(2)')).toEqual(true)
    expect(runNth('nth:sylvester?(3)')).toEqual(false)
    expect(runNth('nth:sylvester?(4)')).toEqual(false)
    expect(runNth('nth:sylvester?(5)')).toEqual(false)
    expect(runNth('nth:sylvester?(6)')).toEqual(true)
    expect(runNth('nth:sylvester?(7)')).toEqual(false)
    expect(runNth('nth:sylvester?(8)')).toEqual(false)
    expect(runNth('nth:sylvester?(9)')).toEqual(false)
    expect(runNth('nth:sylvester?(10650056950806)')).toEqual(true)
  })
})
