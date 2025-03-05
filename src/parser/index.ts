import type { TokenStream } from '../tokenizer/interface'
import { minifyTokenStream } from '../tokenizer/minifyTokenStream'
import { AlgebraicParser } from './AlgebraicParser'
import type { Ast, AstNode, ParseState } from './interface'
import { parsePolishToken } from './PolishTokenParsers'

export function parse(tokenStream: TokenStream): Ast {
  tokenStream = minifyTokenStream(tokenStream, { removeWhiteSpace: true })
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

function parseToken(tokenStream: TokenStream, parseState: ParseState): AstNode {
  return parsePolishToken(tokenStream, parseState)
}
