import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'
import { numberTheoryModule } from '..'
import { LitsError } from '../../../../errors'

const lits = new Lits({ modules: [numberTheoryModule] })

function runNth(code: string) {
  return lits.run(`let nt = import(number-theory); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('pell', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:pell-seq(1)')).toEqual([1])
    expect(runNth('nth:pell-seq(2)')).toEqual([1, 2])
    expect(runNth('nth:pell-seq(3)')).toEqual([1, 2, 5])
    expect(runNth('nth:pell-seq(4)')).toEqual([1, 2, 5, 12])
    expect(runNth('nth:pell-seq(42)')).toEqual([
      1,
      2,
      5,
      12,
      29,
      70,
      169,
      408,
      985,
      2378,
      5741,
      13860,
      33461,
      80782,
      195025,
      470832,
      1136689,
      2744210,
      6625109,
      15994428,
      38613965,
      93222358,
      225058681,
      543339720,
      1311738121,
      3166815962,
      7645370045,
      18457556052,
      44560482149,
      107578520350,
      259717522849,
      627013566048,
      1513744654945,
      3654502875938,
      8822750406821,
      21300003689580,
      51422757785981,
      124145519261542,
      299713796309065,
      723573111879672,
      1746860020068409,
      4217293152016490,
    ])
    expect(() => runNth('nth:pell-seq(0)')).toThrow(LitsError)
    expect(() => runNth('nth:pell-seq(43)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:pell-nth(1)')).toEqual(1)
    expect(runNth('nth:pell-nth(2)')).toEqual(2)
    expect(runNth('nth:pell-nth(3)')).toEqual(5)
    expect(runNth('nth:pell-nth(4)')).toEqual(12)
    expect(runNth('nth:pell-nth(31)')).toEqual(259717522849)
    expect(runNth('nth:pell-nth(42)')).toEqual(4217293152016490)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:pell-take-while(-> $ < 1000)')).toEqual([1, 2, 5, 12, 29, 70, 169, 408, 985])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:pell?(0)')).toEqual(false)
    expect(runNth('nth:pell?(1)')).toEqual(true)
    expect(runNth('nth:pell?(2)')).toEqual(true)
    expect(runNth('nth:pell?(3)')).toEqual(false)
    expect(runNth('nth:pell?(4)')).toEqual(false)
    expect(runNth('nth:pell?(5)')).toEqual(true)
    expect(runNth('nth:pell?(6)')).toEqual(false)
    expect(runNth('nth:pell?(7)')).toEqual(false)
    expect(runNth('nth:pell?(8)')).toEqual(false)
    expect(runNth('nth:pell?(9)')).toEqual(false)
  })
})
