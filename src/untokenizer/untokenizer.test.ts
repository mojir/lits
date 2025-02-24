import { describe, expect, it } from 'vitest'
import { Lits } from '../Lits/Lits'

const lits = new Lits()

const postfixExamples = [
  '(+ 1 2)',
  `
(defn foo [x]
  (+ x 1))`,
  '{ a 1, b 2 }',
  '#(+ % 1)',
  '($ 1 + 2)',
]

const infixExamples = [
  '1 + 2',
  '-1 * (2 - 3)',
  '@(+ 1 2)',
]

describe('untokenizer', () => {
  describe('untokenize', () => {
    it('should untokenize postfix Examples', () => {
      for (const example of postfixExamples) {
        const tokenStream = lits.tokenize(example)
        const result = lits.untokenize(tokenStream)
        expect(result).toBe(example)
      }
    })
    it('should untokenize infix Examples', () => {
      for (const example of infixExamples) {
        const tokenStream = lits.tokenize(example, { infix: true })
        const result = lits.untokenize(tokenStream)
        expect(result).toBe(example)
      }
    })
  })
})
