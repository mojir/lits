import type { TokenStream } from '../tokenizer/interface'
import { isCommentToken, isInfixWhitespaceToken, isPostfixWhitespaceToken } from '../tokenizer/Token'
import type { Ast, ParseState } from './interface'
import { parseToken } from './parsers'

export function parse(tokenStream: TokenStream): Ast {
  const safeTokenStream = removeUnnecessaryTokens(tokenStream)

  const ast: Ast = {
    b: [],
    hasDebugData: safeTokenStream.hasDebugData,
  }

  const parseState: ParseState = {
    position: 0,
    infix: safeTokenStream.infix ?? false,
  }

  while (parseState.position < safeTokenStream.tokens.length) {
    ast.b.push(parseToken(safeTokenStream, parseState))
  }

  return ast
}

function removeUnnecessaryTokens(tokenStream: TokenStream): TokenStream {
  const tokens = tokenStream.tokens.filter((token) => {
    if (isCommentToken(token) || isInfixWhitespaceToken(token) || isPostfixWhitespaceToken(token)) {
      return false
    }
    return true
  })

  return { ...tokenStream, tokens }
}
