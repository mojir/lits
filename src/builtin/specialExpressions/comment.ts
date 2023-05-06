import { AstNode, SpecialExpressionNode } from '../../parser/interface'
import { token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

export const commentSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokens, position, { parseToken }) => {
    let tkn = token.as(tokens[position], `EOF`)

    const node: SpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `comment`,
      params: [],
      token: tkn.debugInfo ? tkn : undefined,
    }

    while (!token.is(tkn, { type: `paren`, value: `)` })) {
      let bodyNode: AstNode
      ;[position, bodyNode] = parseToken(tokens, position)
      node.params.push(bodyNode)
      tkn = token.as(tokens[position], `EOF`)
    }
    return [position + 1, node]
  },
  evaluate: () => null,
  validateArity: () => undefined,
  findUndefinedSymbols: () => new Set(),
}
