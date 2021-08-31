import { Token, Tokenizer } from './interface'
import {
  skipWhiteSpace,
  skipComment,
  tokenizeLeftParen,
  tokenizeReservedName,
  tokenizeName,
  tokenizeNumber,
  tokenizeRightParen,
  tokenizeString,
} from './tokenizers'

const tokenizers: Tokenizer[] = [
  skipComment,
  skipWhiteSpace,
  tokenizeLeftParen,
  tokenizeRightParen,
  tokenizeString,
  tokenizeNumber,
  tokenizeReservedName,
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
