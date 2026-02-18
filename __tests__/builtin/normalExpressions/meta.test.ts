import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import '../../../src/initReferenceData'

describe('misc functions', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    describe('doc', () => {
      it('should return the doc for a function', () => {
        expect((lits.run('doc(>=)') as string).length).toBeGreaterThan(0)
        expect((lits.run('doc(>=(_))') as string).length).toBe(0)
        expect((lits.run('doc(number?)') as string).length).toBeGreaterThan(0)
        expect(lits.run('doc(2)')).toBe('')
        expect(lits.run(`
          let add = (a, b) -> {
            """
            Adds two numbers together.
            Returns the sum of a and b.
            """
            
            a + b
          };
          doc(add)
        `)).toBe('Adds two numbers together.\nReturns the sum of a and b.')
        expect(lits.run(`
          let add = () -> {
            """Escaping\\"""."""
            a + b
          };
          doc(add)
        `)).toBe('Escaping""".')
      })
    })
    describe('arity', () => {
      it('should return the arity of a function', () => {
        expect(lits.run('arity(+)')).toEqual({})
        expect(lits.run('arity(1)')).toEqual({ min: 1, max: 1 })
        expect(lits.run('arity((...x) -> x)')).toEqual({})
      })
    })
  }
})
