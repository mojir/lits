import { isA_WhitespaceToken } from '../tokenizer/algebraic/algebraicTokens'
import type { TokenStream } from '../tokenizer/interface'
import { isP_CommentToken, isP_WhitespaceToken } from '../tokenizer/polish/polishTokens'
import { AlgebraicParser } from './AlgebraicParser'
import type { Ast, AstNode, ParseState } from './interface'
import { parsePolishToken } from './PolishTokenParsers'

export function parse(tokenStream: TokenStream): Ast {
  const safeTokenStream = removeUnnecessaryTokens(tokenStream)

  const ast: Ast = {
    b: [],
    hasDebugData: safeTokenStream.hasDebugData,
  }

  const parseState: ParseState = {
    position: 0,
    algebraic: safeTokenStream.algebraic ?? false,
    parseToken,
  }

  while (parseState.position < safeTokenStream.tokens.length) {
    ast.b.push(parseToken(safeTokenStream, parseState))
  }

  return ast
}

function removeUnnecessaryTokens(tokenStream: TokenStream): TokenStream {
  const tokens = tokenStream.tokens.filter((token) => {
    if (isP_CommentToken(token) || isA_WhitespaceToken(token) || isP_WhitespaceToken(token)) {
      return false
    }
    return true
  })

  return { ...tokenStream, tokens }
}

export function parseToken(tokenStream: TokenStream, parseState: ParseState): AstNode {
  if (parseState.algebraic) {
    const algebraicParser = new AlgebraicParser(tokenStream, parseState)
    return algebraicParser.parse()
  }

  return parsePolishToken(tokenStream, parseState)
}
