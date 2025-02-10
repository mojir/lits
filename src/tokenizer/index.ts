import { TokenType } from '../constants/constants'
import { LitsError } from '../errors'
import { getNextPostfixToken } from './postfix'
import type { MetaToken, Token, TokenStream, TokenizeParams } from './interface'
import { getSugar } from './sugar'
import { getNextInfixToken } from './infix'

export function tokenize(input: string, params: TokenizeParams): TokenStream {
  const debug = !!params.debug
  let mode: 'infix' | 'postfix' = params.infix ? 'infix' : 'postfix'
  let position = 0
  const tokenStream: TokenStream = {
    tokens: [],
    filePath: params.filePath,
    hasDebugData: debug,
  }

  while (position < input.length) {
    const [tokenLength, token] = mode === 'postfix'
      ? getNextPostfixToken(input, position, params)
      : getNextInfixToken(input, position, params)
    position += tokenLength
    if (token) {
      tokenStream.tokens.push(token)
      if (token.t === TokenType.Infix) {
        mode = 'infix'
      }
      if (token.t === TokenType.Postfix) {
        mode = 'postfix'
      }
    }
  }

  applySugar(tokenStream)

  return tokenStream
}

function applySugar(tokenStream: TokenStream) {
  const sugar = getSugar()
  sugar.forEach(sugarFn => sugarFn(tokenStream))
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
