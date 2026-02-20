import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { numberTheoryModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [numberTheoryModule] })

function runNth(code: string) {
  return lits.run(`let nt = import("number-theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('lucky', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:lucky-seq(1)')).toEqual([1])
    expect(runNth('nth:lucky-seq(2)')).toEqual([1, 3])
    expect(runNth('nth:lucky-seq(3)')).toEqual([1, 3, 7])
    expect(runNth('nth:lucky-seq(4)')).toEqual([1, 3, 7, 9])
    expect(runNth('nth:lucky-seq(20)')).toEqual([1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79])
    expect(() => runNth('nth:lucky-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:lucky-nth(1)')).toEqual(1)
    expect(runNth('nth:lucky-nth(2)')).toEqual(3)
    expect(runNth('nth:lucky-nth(3)')).toEqual(7)
    expect(runNth('nth:lucky-nth(4)')).toEqual(9)
    expect(runNth('nth:lucky-nth(5)')).toEqual(13)
    expect(runNth('nth:lucky-nth(6)')).toEqual(15)
    expect(runNth('nth:lucky-nth(7)')).toEqual(21)
    expect(runNth('nth:lucky-nth(8)')).toEqual(25)
    expect(runNth('nth:lucky-nth(20)')).toEqual(79)
    expect(runNth('nth:lucky-nth(3000)')).toEqual(30367)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:lucky-take-while(-> $ < 100)')).toEqual([1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79, 87, 93, 99])
    expect(runNth('nth:lucky-take-while(-> $2 < 3000)')).toBeDefined()
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:lucky?(0)')).toEqual(false)
    expect(runNth('nth:lucky?(1)')).toEqual(true)
    expect(runNth('nth:lucky?(2)')).toEqual(false)
    expect(runNth('nth:lucky?(3)')).toEqual(true)
    expect(runNth('nth:lucky?(4)')).toEqual(false)
    expect(runNth('nth:lucky?(5)')).toEqual(false)
    expect(runNth('nth:lucky?(6)')).toEqual(false)
    expect(runNth('nth:lucky?(7)')).toEqual(true)
    expect(runNth('nth:lucky?(8)')).toEqual(false)
    expect(runNth('nth:lucky?(99)')).toEqual(true)
    expect(runNth('nth:lucky?(100)')).toEqual(false)
  })
})
