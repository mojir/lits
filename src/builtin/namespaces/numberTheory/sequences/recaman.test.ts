import { describe, expect, it } from 'vitest'
import { Lits } from '../../../../Lits/Lits'

const lits = new Lits()

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('recaman', () => {
  it('should return the correct sequence', () => {
    expect(runNth('nth:recaman-seq(1)')).toEqual([0])
    expect(runNth('nth:recaman-seq(2)')).toEqual([0, 1])
    expect(runNth('nth:recaman-seq(3)')).toEqual([0, 1, 3])
    expect(runNth('nth:recaman-seq(4)')).toEqual([0, 1, 3, 6])
    expect(runNth('nth:recaman-seq(5)')).toEqual([0, 1, 3, 6, 2])
    expect(runNth('nth:recaman-seq(6)')).toEqual([0, 1, 3, 6, 2, 7])
    expect(runNth('nth:recaman-seq(71)')).toEqual([0, 1, 3, 6, 2, 7, 13, 20, 12, 21, 11, 22, 10, 23, 9, 24, 8, 25, 43, 62, 42, 63, 41, 18, 42, 17, 43, 16, 44, 15, 45, 14, 46, 79, 113, 78, 114, 77, 39, 78, 38, 79, 37, 80, 36, 81, 35, 82, 34, 83, 33, 84, 32, 85, 31, 86, 30, 87, 29, 88, 28, 89, 27, 90, 26, 91, 157, 224, 156, 225, 155])
  })

  it('should return the correct takeWhile sequence', () => {
    expect(runNth('nth:recaman-take-while(-> $ < 8)')).toEqual([0, 1, 3, 6, 2, 7])
    expect(runNth('nth:recaman-take-while(-> $2 < 10)')).toEqual([0, 1, 3, 6, 2, 7, 13, 20, 12, 21])
    expect(runNth('nth:recaman-take-while(-> false)')).toEqual([])
  })

  it('should determine if numbers are in the sequence', () => {
    expect(runNth('nth:recaman?(0)')).toEqual(true)
    expect(runNth('nth:recaman?(1)')).toEqual(true)
    expect(runNth('nth:recaman?(2)')).toEqual(true)
    expect(runNth('nth:recaman?(3)')).toEqual(true)
    expect(runNth('nth:recaman?(4)')).toEqual(true)
    expect(runNth('nth:recaman?(5)')).toEqual(true)
    expect(runNth('nth:recaman?(6)')).toEqual(true)
    expect(runNth('nth:recaman?(7)')).toEqual(true)
    expect(runNth('nth:recaman?(8)')).toEqual(true)
    expect(runNth('nth:recaman?(9)')).toEqual(true)
  })
})
