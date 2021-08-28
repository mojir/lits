import { Token, Tokenizer } from './Tokenizer.types'
import {
  skipWhiteSpace,
  tokenizeLeftParen,
  tokenizeName,
  tokenizeNumber,
  tokenizeRightParen,
  tokenizeString,
} from './tokenizers'

const tokenizers: Tokenizer[] = [
  skipWhiteSpace,
  tokenizeLeftParen,
  tokenizeRightParen,
  tokenizeString,
  tokenizeNumber,
  tokenizeName,
]

export function tokenize(input: string): Token[] {
  const tokens: Token[] = []
  let position = 0
  let tokenized = false
  while (position < input.length) {
    tokenized = false
    for (const tokenize of tokenizers) {
      const [length, token] = tokenize(input, position)
      if (length > 0) {
        tokenized = true
        position += length
        if (token) {
          tokens.push(token)
          break
        }
      }
    }
    if (!tokenized) {
      throw new SyntaxError(`Unrecognized character at position ${position}: '${input[position]}'`)
    }
  }
  return tokens
}
