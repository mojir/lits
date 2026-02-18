import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { numberTheoryNamespace } from './'

const lits = new Lits({ namespaces: [numberTheoryNamespace] })

function runNth(code: string) {
  return lits.run(`let nt = import("Number-Theory"); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('divisors', () => {
  describe('nth:divisors', () => {
    it('should return divisors of a number', () => {
      expect(runNth('nth:divisors(1)')).toEqual([1])
      expect(runNth('nth:divisors(2)')).toEqual([1, 2])
      expect(runNth('nth:divisors(3)')).toEqual([1, 3])
      expect(runNth('nth:divisors(4)')).toEqual([1, 2, 4])
      expect(runNth('nth:divisors(5)')).toEqual([1, 5])
      expect(runNth('nth:divisors(6)')).toEqual([1, 2, 3, 6])
      expect(runNth('nth:divisors(7)')).toEqual([1, 7])
      expect(runNth('nth:divisors(8)')).toEqual([1, 2, 4, 8])
      expect(runNth('nth:divisors(9)')).toEqual([1, 3, 9])
      expect(runNth('nth:divisors(10)')).toEqual([1, 2, 5, 10])
      expect(runNth('nth:divisors(100)')).toEqual([1, 2, 4, 5, 10, 20, 25, 50, 100])
    })
  })
  describe('nth:count-divisors', () => {
    it('should return the number of divisors of a number', () => {
      expect(runNth('nth:count-divisors(1)')).toEqual(1)
      expect(runNth('nth:count-divisors(2)')).toEqual(2)
      expect(runNth('nth:count-divisors(3)')).toEqual(2)
      expect(runNth('nth:count-divisors(4)')).toEqual(3)
      expect(runNth('nth:count-divisors(5)')).toEqual(2)
      expect(runNth('nth:count-divisors(6)')).toEqual(4)
      expect(runNth('nth:count-divisors(7)')).toEqual(2)
      expect(runNth('nth:count-divisors(8)')).toEqual(4)
      expect(runNth('nth:count-divisors(9)')).toEqual(3)
      expect(runNth('nth:count-divisors(10)')).toEqual(4)
      expect(runNth('nth:count-divisors(100)')).toEqual(9)
    })
  })
  describe('nth:proper-divisors', () => {
    it('should return proper divisors of a number', () => {
      expect(runNth('nth:proper-divisors(1)')).toEqual([])
      expect(runNth('nth:proper-divisors(2)')).toEqual([1])
      expect(runNth('nth:proper-divisors(3)')).toEqual([1])
      expect(runNth('nth:proper-divisors(4)')).toEqual([1, 2])
      expect(runNth('nth:proper-divisors(5)')).toEqual([1])
      expect(runNth('nth:proper-divisors(6)')).toEqual([1, 2, 3])
      expect(runNth('nth:proper-divisors(7)')).toEqual([1])
      expect(runNth('nth:proper-divisors(8)')).toEqual([1, 2, 4])
      expect(runNth('nth:proper-divisors(9)')).toEqual([1, 3])
      expect(runNth('nth:proper-divisors(10)')).toEqual([1, 2, 5])
      expect(runNth('nth:proper-divisors(100)')).toEqual([1, 2, 4, 5, 10, 20, 25, 50])
    })
  })
  describe('nth:count-proper-divisors', () => {
    it('should return the number of proper divisors of a number', () => {
      expect(runNth('nth:count-proper-divisors(1)')).toEqual(0)
      expect(runNth('nth:count-proper-divisors(2)')).toEqual(1)
      expect(runNth('nth:count-proper-divisors(3)')).toEqual(1)
      expect(runNth('nth:count-proper-divisors(4)')).toEqual(2)
      expect(runNth('nth:count-proper-divisors(5)')).toEqual(1)
      expect(runNth('nth:count-proper-divisors(6)')).toEqual(3)
      expect(runNth('nth:count-proper-divisors(7)')).toEqual(1)
      expect(runNth('nth:count-proper-divisors(8)')).toEqual(3)
      expect(runNth('nth:count-proper-divisors(9)')).toEqual(2)
      expect(runNth('nth:count-proper-divisors(10)')).toEqual(3)
      expect(runNth('nth:count-proper-divisors(100)')).toEqual(8)
    })
  })
})
