import { LitsError } from '../errors'
import { getNextPostfixToken } from './postfix'
import type { TokenStream, TokenizeParams } from './interface'
import { getSugar } from './sugar'
import { getNextInfixToken } from './infix'
import { type MetaToken, type Token, isCommentToken, isInfixToken, isNewLineToken, isPostfixToken } from './Token'

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
      if (isInfixToken(token)) {
        infix = true
      }
      if (isPostfixToken(token)) {
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
  return !!token && (isNewLineToken(token) || isCommentToken(token))
}

export function assertMetaToken(token?: Token): asserts token is MetaToken {
  if (!isMetaToken(token))
    throw new LitsError(`Expected meta token, got ${token}.`)
}
