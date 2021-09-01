import { Token } from '../tokenizer/interface'
import { Ast } from './interface'
import { parseToken } from './parsers'

export function parse(tokens: Token[]): Ast {
  const ast: Ast = {
    type: 'Program',
    body: [],
  }
  let position = 0
  while (position < tokens.length) {
    const [newPosition, node] = parseToken(tokens, position)
    position = newPosition
    ast.body.push(node)
  }
  return ast
}
