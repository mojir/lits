import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { numberTheoryModule } from '..'

const lits = new Lits({ modules: [numberTheoryModule] })

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('thueMore', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:thue-morse-seq(1)')).toEqual([0])
    expect(runNth('nth:thue-morse-seq(2)')).toEqual([0, 1])
    expect(runNth('nth:thue-morse-seq(3)')).toEqual([0, 1, 1])
    expect(runNth('nth:thue-morse-seq(4)')).toEqual([0, 1, 1, 0])
    expect(runNth('nth:thue-morse-seq(5)')).toEqual([0, 1, 1, 0, 1])
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:thue-morse-nth(1)')).toEqual(0)
    expect(runNth('nth:thue-morse-nth(2)')).toEqual(1)
    expect(runNth('nth:thue-morse-nth(3)')).toEqual(1)
    expect(runNth('nth:thue-morse-nth(4)')).toEqual(0)
    expect(runNth('nth:thue-morse-nth(5)')).toEqual(1)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:thue-morse-take-while(-> $2 < 5)')).toEqual([0, 1, 1, 0, 1])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:thue-morse?(0)')).toEqual(true)
    expect(runNth('nth:thue-morse?(1)')).toEqual(true)
    expect(runNth('nth:thue-morse?(2)')).toEqual(false)
  })
})
