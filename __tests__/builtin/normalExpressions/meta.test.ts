import { describe, expect, it } from 'vitest'
import { Lits } from '../../../src/Lits/Lits'
import { getMetaNormalExpression } from '../../../src/builtin/core/meta'
import type { ContextStack } from '../../../src/evaluator/ContextStack'
import type { ExecuteFunction } from '../../../src/evaluator/interface'
import { FUNCTION_SYMBOL } from '../../../src/utils/symbols'
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
          let add = (a, b) -> do
            """
            Adds two numbers together.
            Returns the sum of a and b.
            """
            
            a + b
          end;
          doc(add)
        `)).toBe('Adds two numbers together.\nReturns the sum of a and b.')
        expect(lits.run(`
          let add = () -> do
            """Escaping\\"""."""
            a + b
          end;
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

  describe('doc with empty reference', () => {
    it('should return empty string for builtin with no reference data', () => {
      const meta = getMetaNormalExpression({})
      const builtinFn = {
        [FUNCTION_SYMBOL]: true,
        type: 'function' as const,
        functionType: 'Builtin' as const,
        name: '>=',
        overloads: [],
        parameterCount: 2,
        arity: {},
      }
      const result = meta.doc!.evaluate(
        [builtinFn],
        { position: { line: 1, column: 1 }, code: 'doc(>=)' },
        undefined as unknown as ContextStack,
        { executeFunction: undefined as unknown as ExecuteFunction },
      )
      expect(result).toBe('')
    })
  })
})
