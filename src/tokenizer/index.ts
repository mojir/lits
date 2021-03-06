import { LitsError } from '../errors'
import { Token, Tokenizer, SourceCodeInfo } from './interface'
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
  sourceCodeLine: string

  constructor(line: number, column: number, sourceCodeLine: string) {
    this.line = line
    this.column = column
    this.sourceCodeLine = sourceCodeLine
  }

  private get position(): string {
    return `(${this.line}:${this.column})`
  }

  private getMarker(unindent: number): string {
    return `\n${` `.repeat(this.column - 1 - unindent)}^`
  }

  private get debugInfo(): string {
    const unindent = this.sourceCodeLine.replace(/^( *).*/, `$1`).length
    return `\n${this.sourceCodeLine.substr(unindent)}${this.getMarker(unindent)}`
  }

  toString() {
    return `${this.position}${this.debugInfo}`
  }
}

function getSourceCodeLine(input: string, lineNbr: number): string {
  return input.split(/\r\n|\r|\n/)[lineNbr] as string
}

function createSourceCodeInfo(input: string, position: number): SourceCodeInfo {
  const lines = input.substr(0, position + 1).split(/\r\n|\r|\n/)
  const lastLine = lines[lines.length - 1] as string

  const sourceCodeLine = getSourceCodeLine(input, lines.length - 1)
  return new TokenMetaImpl(lines.length, lastLine.length, sourceCodeLine)
}

export function tokenize(input: string, debug: boolean): Token[] {
  const tokens: Token[] = []
  let position = 0
  let tokenized = false
  while (position < input.length) {
    tokenized = false

    // Loop through all tokenizer until one matches
    const sourceCodeInfo: SourceCodeInfo = debug ? createSourceCodeInfo(input, position) : null
    for (const tokenize of tokenizers) {
      const [nbrOfCharacters, token] = tokenize(input, position, sourceCodeInfo)

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
      throw new LitsError(`Unrecognized character '${input[position]}'.`, sourceCodeInfo)
    }
  }
  return tokens
}
