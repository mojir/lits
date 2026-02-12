import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { LitsError } from '../../../errors'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("nth"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('happy', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:happy-seq(1)')).toEqual([1])
    expect(runNth('nth:happy-seq(2)')).toEqual([1, 7])
    expect(runNth('nth:happy-seq(3)')).toEqual([1, 7, 10])
    expect(runNth('nth:happy-seq(4)')).toEqual([1, 7, 10, 13])
    expect(runNth('nth:happy-seq(20)')).toEqual([1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97, 100])
    expect(() => runNth('nth:happy-seq(0)')).toThrow(LitsError)
  })

  it('should return the correct nth term', () => {
    expect(runNth('nth:happy-nth(1)')).toEqual(1)
    expect(runNth('nth:happy-nth(2)')).toEqual(7)
    expect(runNth('nth:happy-nth(3)')).toEqual(10)
    expect(runNth('nth:happy-nth(4)')).toEqual(13)
    expect(runNth('nth:happy-nth(5)')).toEqual(19)
    expect(runNth('nth:happy-nth(6)')).toEqual(23)
    expect(runNth('nth:happy-nth(7)')).toEqual(28)
    expect(runNth('nth:happy-nth(8)')).toEqual(31)
    expect(runNth('nth:happy-nth(20)')).toEqual(100)
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:happy-take-while(-> $ < 100)')).toEqual([1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:happy?(0)')).toEqual(false)
    expect(runNth('nth:happy?(1)')).toEqual(true)
    expect(runNth('nth:happy?(2)')).toEqual(false)
    expect(runNth('nth:happy?(3)')).toEqual(false)
    expect(runNth('nth:happy?(4)')).toEqual(false)
    expect(runNth('nth:happy?(5)')).toEqual(false)
    expect(runNth('nth:happy?(6)')).toEqual(false)
    expect(runNth('nth:happy?(7)')).toEqual(true)
    expect(runNth('nth:happy?(8)')).toEqual(false)
    expect(runNth('nth:happy?(100)')).toEqual(true)
    expect(runNth('nth:happy?(101)')).toEqual(false)
  })
})
