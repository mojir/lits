import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { numberTheoryModule } from './'

const lits = new Lits({ modules: [numberTheoryModule] })

function runNth(code: string) {
  return lits.run(`let nt = import(number-theory); ${code.replace(/nth:/g, 'nt.')}`)
}
describe('combinations', () => {
  describe('nth:combinations', () => {
    it('should return the combinations of n elements from a set', () => {
      expect(runNth('nth:combinations(["a", "b", "c"], 0)')).toEqual([[]])
      expect(runNth('nth:combinations(["a", "b", "c"], 2)')).toEqual([
        ['a', 'b'],
        ['a', 'c'],
        ['b', 'c'],
      ])
      expect(runNth('nth:combinations(["a", "b", "c"], 3)')).toEqual([
        ['a', 'b', 'c'],
      ])
      expect(runNth('nth:combinations(["a", "b", "c"], 1)')).toEqual([
        ['a'],
        ['b'],
        ['c'],
      ])
      expect(runNth('nth:combinations(["a", "b", "c"], 0)')).toEqual([
        [],
      ])
    })
  })
  describe('nth:count-combinations', () => {
    it('should return the number of combinations from n, k', () => {
      expect(runNth('nth:count-combinations(2, 2)')).toEqual(1)
      expect(runNth('nth:count-combinations(3, 2)')).toEqual(3)
      expect(runNth('nth:count-combinations(4, 2)')).toEqual(6)
      expect(runNth('nth:count-combinations(5, 3)')).toEqual(10)
    })
  })
})
