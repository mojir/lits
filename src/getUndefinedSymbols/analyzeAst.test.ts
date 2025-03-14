import { describe, expect, it } from 'vitest'
import { builtin } from '../builtin'
import { createContextStack } from '../evaluator/ContextStack'
import { Lits } from '../Lits/Lits'
import { getUndefinedSymbols } from '.'

describe('analyze', () => {
  describe('getUndefinedSymbols.', () => {
    for (const lits of [new Lits(), new Lits({ debug: true })]) {
      it('example', () => {
        const program = 'a + b'
        const tokens = lits.tokenize(program)
        const ast = lits.parse(tokens)
        expect(getUndefinedSymbols(ast, createContextStack(), builtin)).toEqual(new Set(['a', 'b']))
      })
    }
  })
})
