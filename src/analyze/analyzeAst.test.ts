import { describe, expect, it } from 'vitest'
import { Lits } from '..'
import { builtin } from '../builtin'
import { getUndefinedSymbolNames } from '../../__tests__/testUtils'
import { createContextStack } from '../evaluator/ContextStack'
import { findUnresolvedIdentifiers } from './findUnresolvedIdentifiers'
import type { Analysis } from '.'

describe('analyze', () => {
  describe('findUnresolvedIdentifiers.', () => {
    for (const lits of [new Lits({ polish: true }), new Lits({ debug: true, polish: true })]) {
      it('example', () => {
        const program = '(+ a b)'
        const tokens = lits.tokenize(program)
        const ast = lits.parse(tokens)
        const analyzeResult: Analysis = {
          unresolvedIdentifiers: findUnresolvedIdentifiers(ast, createContextStack(), builtin),
        }
        expect(getUndefinedSymbolNames(analyzeResult)).toEqual(new Set(['a', 'b']))
      })
    }
  })
})
