import { describe, expect, it } from 'vitest'
import { Lits } from '../Lits/Lits'

const polishExamples = [
  '(+ 1 2)',
  `
(defn foo [x]
  (+ x 1))`,
  '{ a 1, b 2 }',
  '#(+ % 1)',
  '($ 1 + 2)',
]

const algebraicExamples = [
  '1 + 2',
  '-1 * (2 - 3)',
]

describe('untokenizer', () => {
  describe('untokenize', () => {
    it('should untokenize polish Examples', () => {
      const lits = new Lits({ polish: true })

      for (const example of polishExamples) {
        const tokenStream = lits.tokenize(example)
        const result = lits.untokenize(tokenStream)
        expect(result).toBe(example)
      }
    })
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
