import { describe, expect, it } from 'vitest'
import { Lits } from '../src'

describe('fnShorthand', () => {
  for (const lits of [new Lits({ polish: true }), new Lits({ debug: true, polish: true })]) {
    it('samples', () => {
      expect(lits.run('(#(identity "Kalle"))')).toBe('Kalle')
      expect(lits.run('(#(str %) 1)')).toBe('1')
      expect(lits.run('(#(str %1) 1)')).toBe('1')
      expect(lits.run('(#(+ % %2) 1 2)')).toBe(3)
      expect(lits.run('(#(+ %1 %2) 1 2)')).toBe(3)
      expect(lits.run('(def x 10) (#(+ %1 %2 x) 1 2)')).toBe(13)
      expect(lits.run('(#(+ %1 %20) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)')).toBe(21)
      expect(lits.run('(#(+ %1 %1 %2) 1 2)')).toBe(4)
      expect(() => lits.run('(#(+ % %1 %2) 1 2)')).toThrow()
      expect(() => lits.run('#(+ %1 %21)')).toThrow()
      expect(() => lits.run('(#(+ %1 %2) 1)')).toThrow()
      expect(() => lits.run('(#(+ %1 %2) 1)')).toThrow()
      expect(() => lits.run('(#(+ (#(+ %1 1) 1) %2) 1 2)')).toThrow()
    })
  }
})
