import { isA_CommentToken, isA_MultiLineCommentToken, isA_WhitespaceToken } from '../tokenizer/algebraic/algebraicTokens'
import type { TokenStream } from '../tokenizer/interface'
import { isP_CommentToken, isP_WhitespaceToken } from '../tokenizer/polish/polishTokens'
import { AlgebraicParser } from './AlgebraicParser'
import type { Ast, AstNode, ParseState } from './interface'
import { parsePolishToken } from './PolishTokenParsers'

export function parse(tokenStream: TokenStream): Ast {
  tokenStream = removeUnnecessaryTokens(tokenStream)
  const algebraic = tokenStream.algebraic

  const ast: Ast = {
    b: [],
    hasDebugData: tokenStream.hasDebugData,
  }

  const parseState: ParseState = {
    position: 0,
    parseToken,
  }

  if (algebraic) {
    const algebraicParser = new AlgebraicParser(tokenStream, parseState)
    ast.b = algebraicParser.parse()
  }
  else {
    while (parseState.position < tokenStream.tokens.length) {
      ast.b.push(parseToken(tokenStream, parseState))
    }
  }

  return ast
}

function removeUnnecessaryTokens(tokenStream: TokenStream): TokenStream {
  const tokens = tokenStream.tokens.filter((token) => {
    if (
      isP_CommentToken(token)
      || isA_CommentToken(token)
      || isA_MultiLineCommentToken(token)
      || isA_WhitespaceToken(token)
      || isP_WhitespaceToken(token)) {
      return false
    }
    return true
  })

  return { ...tokenStream, tokens }
}

function parseToken(tokenStream: TokenStream, parseState: ParseState): AstNode {
  return parsePolishToken(tokenStream, parseState)
}
