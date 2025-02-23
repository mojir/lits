import { isIF_WhitespaceToken } from '../tokenizer/infix/infixTokens'
import type { TokenStream } from '../tokenizer/interface'
import { isPF_CommentToken, isPF_WhitespaceToken } from '../tokenizer/postfix/postfixTokens'
import { InfixParser } from './InfixParser'
import type { Ast, AstNode, ParseState } from './interface'
import { parsePostfixToken } from './postfixTokenParsers'

export function parse(tokenStream: TokenStream): Ast {
  const safeTokenStream = removeUnnecessaryTokens(tokenStream)

  const ast: Ast = {
    b: [],
    hasDebugData: safeTokenStream.hasDebugData,
  }

  const parseState: ParseState = {
    position: 0,
    infix: safeTokenStream.infix ?? false,
    parseToken,
  }

  while (parseState.position < safeTokenStream.tokens.length) {
    ast.b.push(parseToken(safeTokenStream, parseState))
  }

  return ast
}

function removeUnnecessaryTokens(tokenStream: TokenStream): TokenStream {
  const tokens = tokenStream.tokens.filter((token) => {
    if (isPF_CommentToken(token) || isIF_WhitespaceToken(token) || isPF_WhitespaceToken(token)) {
      return false
    }
    return true
  })

  return { ...tokenStream, tokens }
}

export function parseToken(tokenStream: TokenStream, parseState: ParseState): AstNode {
  if (parseState.infix) {
    const infixParser = new InfixParser(tokenStream, parseState)
    return infixParser.parse()
  }

  return parsePostfixToken(tokenStream, parseState)
}
