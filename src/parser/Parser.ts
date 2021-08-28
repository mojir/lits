import { Token } from '../tokenizer/Tokenizer.types'
import { Ast } from './Parser.types'
import { parseToken } from './parsers'

export function parseProgram(tokens: Token[]): Ast {
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
