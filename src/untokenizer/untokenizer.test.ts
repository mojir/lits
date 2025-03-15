import { describe, expect, it } from 'vitest'
import { Lits } from '../Lits/Lits'

const examples = [
  '1 + 2',
  '-1 * (2 - 3)',
]

describe('untokenizer', () => {
  describe('untokenize', () => {
    it('should untokenize Examples', () => {
      const lits = new Lits()
      for (const example of examples) {
        const tokenStream = lits.tokenize(example)
        const result = lits.untokenize(tokenStream)
        expect(result).toBe(example)
      }
    })
  })
})
