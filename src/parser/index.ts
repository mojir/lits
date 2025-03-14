import type { TokenStream } from '../tokenizer/interface'
import { minifyTokenStream } from '../tokenizer/minifyTokenStream'
import { AlgebraicParser } from './AlgebraicParser'
import type { Ast, AstNode, ParseState } from './interface'
import { parsePolishToken } from './PolishTokenParsers'

export function parse(tokenStream: TokenStream): Ast {
  tokenStream = minifyTokenStream(tokenStream, { removeWhiteSpace: true })
  const ast: Ast = {
    b: [],
    hasDebugData: tokenStream.hasDebugData,
  }

  const parseState: ParseState = {
    position: 0,
    parseToken,
  }

  const algebraicParser = new AlgebraicParser(tokenStream, parseState)
  ast.b = algebraicParser.parse()

  return ast
}

function parseToken(tokenStream: TokenStream, parseState: ParseState): AstNode {
  return parsePolishToken(tokenStream, parseState)
}
