import type { TokenStream } from '../tokenizer/tokenize'
import { minifyTokenStream } from '../tokenizer/minifyTokenStream'
import { AlgebraicParser } from './AlgebraicParser'
import type { Ast, ParseState } from './interface'

export function parse(tokenStream: TokenStream): Ast {
  tokenStream = minifyTokenStream(tokenStream, { removeWhiteSpace: true })
  const ast: Ast = {
    b: [],
    hasDebugData: tokenStream.hasDebugData,
  }

  const parseState: ParseState = {
    position: 0,
  }

  const algebraicParser = new AlgebraicParser(tokenStream, parseState)
  ast.b = algebraicParser.parse()

  return ast
}
