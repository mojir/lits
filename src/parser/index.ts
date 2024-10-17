import type { TokenStream } from '../tokenizer/interface'
import type { Ast, AstNode } from './interface'
import { parseToken } from './parsers'

export function parse(tokenStream: TokenStream): Ast {
  const ast: Ast = {
    b: [],
    hasDebugData: tokenStream.hasDebugData,
  }

  let position = 0
  let node: AstNode
  while (position < tokenStream.tokens.length) {
    ;[position, node] = parseToken(tokenStream, position)
    ast.b.push(node)
  }

  return ast
}
