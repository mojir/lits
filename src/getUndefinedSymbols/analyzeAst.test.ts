import { describe, expect, it } from 'vitest'
import { Lits } from '../Lits/Lits'

describe('analyze', () => {
  describe('getUndefinedSymbols.', () => {
    for (const lits of [new Lits(), new Lits({ debug: true })]) {
      it('example', () => {
        const program = 'a + b'
        const tokens = lits.tokenize(program, { minify: true })
        const ast = lits.parse(tokens)
        expect(lits.getUndefinedSymbols(ast)).toEqual(new Set(['a', 'b']))
      })
    }
  })
})
