import type { TokenStream } from '../tokenizer/interface'
import type { Ast, ParseState } from './interface'
import { parseToken } from './parsers'

export function parse(tokenStream: TokenStream): Ast {
  const ast: Ast = {
    b: [],
    hasDebugData: tokenStream.hasDebugData,
  }

  const parseState: ParseState = {
    position: 0,
    infix: tokenStream.infix ?? false,
  }

  while (parseState.position < tokenStream.tokens.length) {
    ast.b.push(parseToken(tokenStream, parseState))
  }

  return ast
}
