import { TokenType } from '../constants/constants'
import { LitsError } from '../errors'
import type { MetaToken, SourceCodeInfo, Token, TokenDebugData, TokenStream, TokenizeParams, Tokenizer } from './interface'
import { getSugar } from './sugar'
import {
  skipWhiteSpace,
  tokenizeCollectionAccessor,
  tokenizeComment,
  tokenizeFnShorthand,
  tokenizeLeftBracket,
  tokenizeLeftCurly,
  tokenizeLeftParen,
  tokenizeModifier,
  tokenizeName,
  tokenizeNewLine,
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
  skipWhiteSpace,
  tokenizeComment,
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
  const lines = input.substring(0, position + 1).split(/\r\n|\r|\n/)
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
  const debug = !!params.debug
  const tokens: Token[] = []
  let position = 0
  let tokenized = false
  while (position < input.length) {
    tokenized = false

    let leadingNewLineTokens: MetaToken[]
    [position, leadingNewLineTokens] = readLeadingNewLineTokens(input, position, params)
    if (position >= input.length)
      break

    let leadingCommentTokens: MetaToken[]
    [position, leadingCommentTokens] = readLeadingCommentTokens(input, position, params)
    if (position >= input.length)
      break

    const leadingMetaTokens: MetaToken[] = [...leadingNewLineTokens, ...leadingCommentTokens]

    // Loop through all tokenizer until one matches
    const debugData: TokenDebugData | undefined = debug
      ? {
          sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
          metaTokens: { inlineCommentToken: null, leadingMetaTokens },
        }
      : undefined
    for (const tokenizer of tokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position, debugData)

      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
        if (token) {
          let inlineCommentToken: Token<TokenType.Comment> | null = null
          if (!isCommentToken(token))
            [position, inlineCommentToken] = readInlineCommentToken(input, position, params)

          if (token.debugData)
            token.debugData.metaTokens.inlineCommentToken = inlineCommentToken

          if (!isCommentToken(token) || debug)
            tokens.push(token)
        }

        break
      }
    }
    if (!tokenized)
      throw new LitsError(`Unrecognized character '${input[position]}'.`, debugData?.sourceCodeInfo)
  }

  const tokenStream: TokenStream = {
    tokens,
    filePath: params.filePath,
    hasDebugData: debug,
  }

  applySugar(tokenStream)

  return tokenStream
}

function applySugar(tokenStream: TokenStream) {
  const sugar = getSugar()
  sugar.forEach(sugarFn => sugarFn(tokenStream))
}

const newLineTokenizers = [
  tokenizeNewLine,
  skipWhiteSpace,
]

function readLeadingNewLineTokens(input: string, position: number, params: TokenizeParams): [number, MetaToken[]] {
  const newLineTokens: Token<TokenType.NewLine>[] = []

  let tokenized = false
  while (position < input.length) {
    tokenized = false

    const debugData: TokenDebugData | undefined = params.debug
      ? {
          sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
          metaTokens: { inlineCommentToken: null, leadingMetaTokens: [] },
        }
      : undefined

    // Loop through all tokenizer until one matches
    for (const tokenizer of newLineTokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position, debugData)
      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
        if (token) {
          assertNewLineToken(token)

          if (newLineTokens.length < 2)
            newLineTokens.push(token)
        }
        break
      }
    }
    if (!tokenized)
      // All newline tokens read!
      return [position, newLineTokens]
  }
  // Ending up here means that no non newline token was found. I.e. this cannot be leading newline tokens
  return [position, []]
}

const metaTokenizers = [
  tokenizeNewLine,
  skipWhiteSpace,
  tokenizeComment,
]

function readLeadingCommentTokens(input: string, position: number, params: TokenizeParams): [number, MetaToken[]] {
  const commentTokens: Token<TokenType.Comment>[] = []

  const rollbackPosition = position
  let tokenized = false
  while (position < input.length) {
    tokenized = false

    const debugData: TokenDebugData | undefined = params.debug
      ? {
          sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
          metaTokens: { inlineCommentToken: null, leadingMetaTokens: [] },
        }
      : undefined

    // Loop through all tokenizer until one matches
    for (const tokenizer of metaTokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position, debugData)
      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
        if (token) {
          assertMetaToken(token)

          // If a newline token is found, then this is not a leading comment
          if (isNewLineToken(token))
            return [rollbackPosition, []]

          commentTokens.push(token)
        }
        break
      }
    }
    if (!tokenized)
      // All metatokens read!
      return [position, commentTokens]
  }
  // Ending up here means that no non meta token was found. I.e. this cannot be leading meta tokens
  return [rollbackPosition, []]
}

const commentTokenizers = [
  tokenizeNewLine,
  skipWhiteSpace,
  tokenizeComment,
]

function readInlineCommentToken(input: string, position: number, params: TokenizeParams): [number, Token<TokenType.Comment> | null] {
  const rollbackPosition = position
  let tokenized = false
  while (position < input.length) {
    tokenized = false
    const debugData: TokenDebugData | undefined = params.debug
      ? {
          sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
          metaTokens: { inlineCommentToken: null, leadingMetaTokens: [] },
        }
      : undefined

    // Loop through all tokenizer until one matches
    for (const tokenizer of commentTokenizers) {
      const [nbrOfCharacters, token] = tokenizer(input, position, debugData)

      // tokenizer matched
      if (nbrOfCharacters > 0) {
        tokenized = true
        position += nbrOfCharacters
        if (token) {
          if (isNewLineToken(token))
            return [rollbackPosition, null]
          assertCommentToken(token)
          return [position, token]
        }
        break
      }
    }
    if (!tokenized)
      // All metatokens read! Return undefined if not debug mode
      return [rollbackPosition, null]
  }
  // Ending up here means that no comment token was found and end of tokens reached
  return [position, null]
}

export function isMetaToken(token?: Token): token is MetaToken {
  return !!token && (token.t === TokenType.NewLine || token.t === TokenType.Comment)
}

export function assertMetaToken(token?: Token): asserts token is MetaToken {
  if (!isMetaToken(token))
    throw new LitsError(`Expected meta token, got ${token?.t}.`)
}

export function isCommentToken(token?: Token): token is Token<TokenType.Comment> {
  return !!token && token.t === TokenType.Comment
}

export function assertCommentToken(token?: Token): asserts token is Token<TokenType.Comment> {
  if (!isCommentToken(token))
    throw new LitsError(`Expected comment token, got ${token?.t}.`)
}

export function isNewLineToken(token?: Token): token is Token<TokenType.NewLine> {
  return !!token && token.t === TokenType.NewLine
}

export function assertNewLineToken(token?: Token): asserts token is Token<TokenType.NewLine> {
  if (!isNewLineToken(token))
    throw new LitsError(`Expected newline token, got ${token?.t}.`)
}
