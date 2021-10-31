import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface doSpecialExpressionNode extends SpecialExpressionNode {
  name: `do`
}

export const doSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseToken }) => {
    const node: doSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `do`,
      params: [],
      token: asNotUndefined(tokens[position]),
    }

    let token = asNotUndefined(tokens[position])
    while (!(token.type === `paren` && token.value === `)`)) {
      const [newPosition, bodyNode] = parseToken(tokens, position)
      node.params.push(bodyNode)
      position = newPosition
      token = asNotUndefined(tokens[position])
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castDoExpressionNode(node)
    const newContext: Context = {}

    const newContextStack = contextStack.withContext(newContext)
    let result: Any = null
    for (const form of node.params) {
      result = evaluateAstNode(form, newContextStack)
    }
    return result
  },
}

function castDoExpressionNode(_node: SpecialExpressionNode): asserts _node is doSpecialExpressionNode {
  return
}
