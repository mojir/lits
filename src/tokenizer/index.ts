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

// All tokenizers, order matters!
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

    // Loop through all tokenizer until one matches
    for (const tokenize of tokenizers) {
      const [nbrOfCharacters, token] = tokenize(input, position)

      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
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
