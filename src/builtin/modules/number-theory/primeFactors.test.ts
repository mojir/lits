import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { numberTheoryModule } from './'

const lits = new Lits({ modules: [numberTheoryModule] })

function runNth(code: string) {
  return lits.run(`let nt = import(number-theory); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('primeFactors', () => {
  describe('nth:prime-factors', () => {
    it('should return find prime factors', () => {
      expect(runNth('nth:prime-factors(1)')).toEqual([])
      expect(runNth('nth:prime-factors(2)')).toEqual([2])
      expect(runNth('nth:prime-factors(3)')).toEqual([3])
      expect(runNth('nth:prime-factors(4)')).toEqual([2, 2])
      expect(runNth('nth:prime-factors(5)')).toEqual([5])
      expect(runNth('nth:prime-factors(6)')).toEqual([2, 3])
      expect(runNth('nth:prime-factors(7)')).toEqual([7])
      expect(runNth('nth:prime-factors(8)')).toEqual([2, 2, 2])
      expect(runNth('nth:prime-factors(9)')).toEqual([3, 3])
      expect(runNth('nth:prime-factors(10)')).toEqual([2, 5])
      expect(runNth('nth:prime-factors(100)')).toEqual([2, 2, 5, 5])
      expect(runNth('nth:prime-factors(1484147626962)')).toEqual([2, 3, 7, 11, 13, 17, 19, 23, 29, 31, 37])
    })
  })
  describe('nth:distinct-prime-factors', () => {
    it('should return distinct prime factors', () => {
      expect(runNth('nth:distinct-prime-factors(1)')).toEqual([])
      expect(runNth('nth:distinct-prime-factors(2)')).toEqual([2])
      expect(runNth('nth:distinct-prime-factors(3)')).toEqual([3])
      expect(runNth('nth:distinct-prime-factors(4)')).toEqual([2])
      expect(runNth('nth:distinct-prime-factors(5)')).toEqual([5])
      expect(runNth('nth:distinct-prime-factors(6)')).toEqual([2, 3])
      expect(runNth('nth:distinct-prime-factors(7)')).toEqual([7])
      expect(runNth('nth:distinct-prime-factors(8)')).toEqual([2])
      expect(runNth('nth:distinct-prime-factors(9)')).toEqual([3])
    })
  })
  describe('nth:count-prime-factors', () => {
    it('should return the number of prime factors of n', () => {
      expect(runNth('nth:count-prime-factors(1)')).toEqual(0)
      expect(runNth('nth:count-prime-factors(2)')).toEqual(1)
      expect(runNth('nth:count-prime-factors(3)')).toEqual(1)
      expect(runNth('nth:count-prime-factors(4)')).toEqual(2)
      expect(runNth('nth:count-prime-factors(5)')).toEqual(1)
      expect(runNth('nth:count-prime-factors(6)')).toEqual(2)
      expect(runNth('nth:count-prime-factors(7)')).toEqual(1)
      expect(runNth('nth:count-prime-factors(8)')).toEqual(3)
      expect(runNth('nth:count-prime-factors(9)')).toEqual(2)
      expect(runNth('nth:count-prime-factors(10)')).toEqual(2)
      expect(runNth('nth:count-prime-factors(100)')).toEqual(4)
    })
  })
  describe('nth:count-distinct-prime-factors', () => {
    it('should return the number of distinct prime factors of n', () => {
      expect(runNth('nth:count-distinct-prime-factors(1)')).toEqual(0)
      expect(runNth('nth:count-distinct-prime-factors(2)')).toEqual(1)
      expect(runNth('nth:count-distinct-prime-factors(3)')).toEqual(1)
      expect(runNth('nth:count-distinct-prime-factors(4)')).toEqual(1)
      expect(runNth('nth:count-distinct-prime-factors(5)')).toEqual(1)
      expect(runNth('nth:count-distinct-prime-factors(6)')).toEqual(2)
      expect(runNth('nth:count-distinct-prime-factors(7)')).toEqual(1)
      expect(runNth('nth:count-distinct-prime-factors(8)')).toEqual(1)
      expect(runNth('nth:count-distinct-prime-factors(9)')).toEqual(1)
      expect(runNth('nth:count-distinct-prime-factors(10)')).toEqual(2)
      expect(runNth('nth:count-distinct-prime-factors(100)')).toEqual(2)
    })
  })
})
