import { describe, expect, test } from 'vitest'
import { tokenize } from '../tokenizer/tokenize'
import { Lits } from '../Lits/Lits'

describe('typeGuards index file', () => {
  test('transformSymbolTokens', () => {
    const lits = new Lits()
    const tokenStream = tokenize('a + b', false, undefined)
    expect(lits.transformSymbols(tokenStream, s => s.toUpperCase()).tokens).toEqual([
      ['Symbol', 'A'],
      ['Whitespace', ' '],
      ['Operator', '+'],
      ['Whitespace', ' '],
      ['Symbol', 'B'],
    ])
  })
})
