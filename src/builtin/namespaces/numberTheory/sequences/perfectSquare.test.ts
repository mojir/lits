import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { numberTheoryNamespace } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ namespaces: [numberTheoryNamespace] })

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('perfect-square', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:perfect-square-seq(1)')).toEqual([1])
    expect(runNth('nth:perfect-square-seq(2)')).toEqual([1, 4])
    expect(runNth('nth:perfect-square-seq(3)')).toEqual([1, 4, 9])
    expect(runNth('nth:perfect-square-seq(4)')).toEqual([1, 4, 9, 16])
    expect(() => runNth('nth:perfect-square-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:perfect-square-nth(1)')).toEqual(1)
    expect(runNth('nth:perfect-square-nth(2)')).toEqual(4)
    expect(runNth('nth:perfect-square-nth(3)')).toEqual(9)
    expect(runNth('nth:perfect-square-nth(4)')).toEqual(16)
    expect(runNth('nth:perfect-square-nth(5)')).toEqual(25)
    expect(runNth('nth:perfect-square-nth(100)')).toEqual(10000)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:perfect-square-take-while(-> $ < 100)')).toEqual([
      1,
      4,
      9,
      16,
      25,
      36,
      49,
      64,
      81,
    ])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:perfect-square?(0)')).toEqual(false)
    expect(runNth('nth:perfect-square?(1)')).toEqual(true)
    expect(runNth('nth:perfect-square?(2)')).toEqual(false)
    expect(runNth('nth:perfect-square?(3)')).toEqual(false)
    expect(runNth('nth:perfect-square?(4)')).toEqual(true)
    expect(runNth('nth:perfect-square?(5)')).toEqual(false)
    expect(runNth('nth:perfect-square?(6)')).toEqual(false)
    expect(runNth('nth:perfect-square?(7)')).toEqual(false)
    expect(runNth('nth:perfect-square?(8)')).toEqual(false)
    expect(runNth('nth:perfect-square?(9)')).toEqual(true)
    expect(runNth('nth:perfect-square?(100)')).toEqual(true)
    expect(runNth('nth:perfect-square?(1000)')).toEqual(false)
  })
})
