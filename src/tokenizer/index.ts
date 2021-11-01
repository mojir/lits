import { LitsError } from '../errors'
import { Token, Tokenizer, TokenMeta } from './interface'
import {
  skipComment,
  skipWhiteSpace,
  tokenizeLeftBracket,
  tokenizeLeftCurly,
  tokenizeLeftParen,
  tokenizeModifier,
  tokenizeName,
  tokenizeNumber,
  tokenizeReservedName,
  tokenizeRightBracket,
  tokenizeRightCurly,
  tokenizeRightParen,
  tokenizeString,
  tokenizeRegexpShorthand,
  tokenizeFnShorthand,
  tokenizeSymbolString,
} from './tokenizers'

// All tokenizers, order matters!
const tokenizers: Tokenizer[] = [
  skipComment,
  skipWhiteSpace,
  tokenizeLeftParen,
  tokenizeRightParen,
  tokenizeLeftBracket,
  tokenizeRightBracket,
  tokenizeLeftCurly,
  tokenizeRightCurly,
  tokenizeString,
  tokenizeSymbolString,
  tokenizeNumber,
  tokenizeReservedName,
  tokenizeName,
  tokenizeModifier,
  tokenizeRegexpShorthand,
  tokenizeFnShorthand,
]

class TokenMetaImpl {
  line: number
  column: number
  constructor(line: number, column: number) {
    this.line = line
    this.column = column
  }
  toString() {
    return `(${this.line}:${this.column})`
  }
}
export function calculateMeta(input: string, position: number): TokenMeta {
  const lines = input.substr(0, position + 1).split(/\r\n|\r|\n/)
  return new TokenMetaImpl(lines.length, (lines[lines.length - 1] as string).length)
}

export function tokenize(input: string): Token[] {
  const tokens: Token[] = []
  let position = 0
  let tokenized = false
  while (position < input.length) {
    tokenized = false

    // Loop through all tokenizer until one matches
    const meta: TokenMeta = calculateMeta(input, position)
    for (const tokenize of tokenizers) {
      const [nbrOfCharacters, token] = tokenize(input, position, meta)

      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
        if (token) {
          tokens.push(token)
        }
        break
      }
    }
    if (!tokenized) {
      throw new LitsError(`Unrecognized character '${input[position]}'`, meta)
    }
  }
  return tokens
}
