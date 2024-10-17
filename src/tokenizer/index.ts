import { LitsError } from '../errors'
import type { SourceCodeInfo, Token, TokenStream, TokenizeParams, Tokenizer } from './interface'
import { getSugar } from './sugar'
import {
  skipComment,
  skipWhiteSpace,
  tokenizeCollectionAccessor,
  tokenizeFnShorthand,
  tokenizeLeftBracket,
  tokenizeLeftCurly,
  tokenizeLeftParen,
  tokenizeModifier,
  tokenizeName,
  tokenizeNumber,
  tokenizeRegexpShorthand,
  tokenizeReservedName,
  tokenizeRightBracket,
  tokenizeRightCurly,
  tokenizeRightParen,
  tokenizeString,
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
  tokenizeCollectionAccessor,
]

function getSourceCodeLine(input: string, lineNbr: number): string {
  return input.split(/\r\n|\r|\n/)[lineNbr] as string
}

function createSourceCodeInfo(input: string, position: number, filePath?: string): SourceCodeInfo {
  const lines = input.substr(0, position + 1).split(/\r\n|\r|\n/)
  const lastLine = lines[lines.length - 1] as string

  const code = getSourceCodeLine(input, lines.length - 1)
  const line = lines.length
  const column = lastLine.length
  return {
    code,
    position: {
      line,
      column,
    },
    filePath,
  }
}

export function tokenize(input: string, params: TokenizeParams): TokenStream {
  const tokens: Token[] = []
  let position = 0
  let tokenized = false
  while (position < input.length) {
    tokenized = false

    // Loop through all tokenizer until one matches
    const sourceCodeInfo: SourceCodeInfo | undefined = params.debug
      ? createSourceCodeInfo(input, position, params.filePath)
      : undefined
    for (const tokenizer of tokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position, sourceCodeInfo)

      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
        if (token)
          tokens.push(token)

        break
      }
    }
    if (!tokenized)
      throw new LitsError(`Unrecognized character '${input[position]}'.`, sourceCodeInfo)
  }

  const tokenStream = {
    tokens,
    filePath: params.filePath,
  }

  applySugar(tokenStream)

  return tokenStream
}

function applySugar(tokenStream: TokenStream) {
  const sugar = getSugar()
  sugar.forEach(sugarFn => sugarFn(tokenStream))
}
