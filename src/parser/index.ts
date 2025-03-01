import { AstNodeType } from '../constants/constants'
import { isA_CommentToken, isA_MultiLineCommentToken, isA_WhitespaceToken } from '../tokenizer/algebraic/algebraicTokens'
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

export function parseToken(tokenStream: TokenStream, parseState: ParseState): AstNode {
  if (parseState.algebraic) {
    const algebraicParser = new AlgebraicParser(tokenStream, parseState)
    const nodes = algebraicParser.parse()
    if (nodes.length === 1) {
      return nodes[0]!
    }
    return {
      t: AstNodeType.SpecialExpression,
      n: 'do',
      p: nodes,
      token: nodes[0]!.token,
    }
  }

  return parsePolishToken(tokenStream, parseState)
}
