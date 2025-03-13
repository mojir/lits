import { describe, expect, it } from 'vitest'
import { Lits } from '../Lits/Lits'

const algebraicExamples = [
  '1 + 2',
  '-1 * (2 - 3)',
]

describe('untokenizer', () => {
  describe('untokenize', () => {
    it('should untokenize algebraic Examples', () => {
      const lits = new Lits()
      for (const example of algebraicExamples) {
        const tokenStream = lits.tokenize(example)
        const result = lits.untokenize(tokenStream)
        expect(result).toBe(example)
      }
    })
  })
})
