import { describe, expect, it } from 'vitest'
import { Lits } from '..'
import { builtin } from '../builtin'
import { getUndefinedSymbolNames } from '../../__tests__/testUtils'
import { createContextStack } from '../evaluator/ContextStack'
import { findUnresolvedSymbols } from './findUnresolvedSymbols'
import type { Analysis } from '.'

describe('analyze', () => {
  describe('findUnresolvedSymbols.', () => {
    for (const lits of [new Lits({ polish: true }), new Lits({ debug: true, polish: true })]) {
      it('example', () => {
        const program = '(+ a b)'
        const tokens = lits.tokenize(program)
        const ast = lits.parse(tokens)
        const analyzeResult: Analysis = {
          unresolvedSymbols: findUnresolvedSymbols(ast, createContextStack(), builtin),
        }
        expect(getUndefinedSymbolNames(analyzeResult)).toEqual(new Set(['a', 'b']))
      })
    }
  })
})
