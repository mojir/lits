import { Context } from '../../ContextStack/interface'
import { Any } from '../../interface'
import { AstNode, SpecialExpressionNode } from '../../parser/interface'
import { token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

export const doSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseToken }) => {
    let tkn = token.as(tokens[position], `EOF`)

    const node: SpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `do`,
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
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const newContext: Context = {}

    const newContextStack = contextStack.withContext(newContext)
    let result: Any = null
    for (const form of node.params) {
      result = evaluateAstNode(form, newContextStack)
    }
    return result
  },
  validateArity: () => undefined,
  findUndefinedSymbols: (node, contextStack, { findUndefinedSymbols, builtin }) =>
    findUndefinedSymbols(node.params, contextStack, builtin),
}
