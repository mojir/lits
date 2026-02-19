import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { numberTheoryModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [numberTheoryModule] })

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('perfect', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:perfect-seq(1)')).toEqual([6])
    expect(runNth('nth:perfect-seq(2)')).toEqual([6, 28])
    expect(runNth('nth:perfect-seq(3)')).toEqual([6, 28, 496])
    expect(runNth('nth:perfect-seq(4)')).toEqual([6, 28, 496, 8128])
    expect(runNth('nth:perfect-seq(7)')).toEqual([6, 28, 496, 8128, 33550336, 8589869056, 137438691328])
    expect(runNth('nth:perfect-seq()')).toEqual([6, 28, 496, 8128, 33550336, 8589869056, 137438691328])
    expect(() => runNth('nth:perfect-seq(0)')).toThrow(LitsError)
    expect(() => runNth('nth:perfect-seq(20)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:perfect-nth(1)')).toEqual(6)
    expect(runNth('nth:perfect-nth(2)')).toEqual(28)
    expect(runNth('nth:perfect-nth(3)')).toEqual(496)
    expect(runNth('nth:perfect-nth(4)')).toEqual(8128)
    expect(runNth('nth:perfect-nth(7)')).toEqual(137438691328)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:perfect-take-while(-> $ < 1000)')).toEqual([6, 28, 496])
    expect(runNth('nth:perfect-take-while(-> true)')).toEqual([6, 28, 496, 8128, 33550336, 8589869056, 137438691328])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:perfect?(0)')).toEqual(false)
    expect(runNth('nth:perfect?(1)')).toEqual(false)
    expect(runNth('nth:perfect?(2)')).toEqual(false)
    expect(runNth('nth:perfect?(3)')).toEqual(false)
    expect(runNth('nth:perfect?(4)')).toEqual(false)
    expect(runNth('nth:perfect?(5)')).toEqual(false)
    expect(runNth('nth:perfect?(6)')).toEqual(true)
    expect(runNth('nth:perfect?(7)')).toEqual(false)
    expect(runNth('nth:perfect?(8)')).toEqual(false)
    expect(runNth('nth:perfect?(9)')).toEqual(false)
    expect(runNth('nth:perfect?(137438691328)')).toEqual(true)
    expect(runNth('nth:perfect?(137438691329)')).toEqual(false)
  })
})
