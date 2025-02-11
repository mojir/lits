import { LitsError } from '../errors'
import { getNextPostfixToken } from './postfix'
import type { MetaToken, Token, TokenStream, TokenizeParams } from './interface'
import { getSugar } from './sugar'
import { getNextInfixToken } from './infix'

export function tokenize(input: string, params: TokenizeParams): TokenStream {
  const debug = !!params.debug
  let infix = !!params.infix
  let position = 0
  const tokenStream: TokenStream = {
    tokens: [],
    filePath: params.filePath,
    hasDebugData: debug,
    infix,
  }

  while (position < input.length) {
    const [count, token] = infix
      ? getNextInfixToken(input, position, params)
      : getNextPostfixToken(input, position, params)

    position += count
    if (token) {
      tokenStream.tokens.push(token)
      if (token.t === 'Infix') {
        infix = true
      }
      if (token.t === 'Postfix') {
        infix = false
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
  return !!token && (token.t === 'NewLine' || token.t === 'Comment')
}

export function assertMetaToken(token?: Token): asserts token is MetaToken {
  if (!isMetaToken(token))
    throw new LitsError(`Expected meta token, got ${token?.t}.`)
}

export function isCommentToken(token?: Token): token is Token<'Comment'> {
  return !!token && token.t === 'Comment'
}

export function assertCommentToken(token?: Token): asserts token is Token<'Comment'> {
  if (!isCommentToken(token))
    throw new LitsError(`Expected comment token, got ${token?.t}.`)
}

export function isNewLineToken(token?: Token): token is Token<'NewLine'> {
  return !!token && token.t === 'NewLine'
}

export function assertNewLineToken(token?: Token): asserts token is Token<'NewLine'> {
  if (!isNewLineToken(token))
    throw new LitsError(`Expected newline token, got ${token?.t}.`)
}
