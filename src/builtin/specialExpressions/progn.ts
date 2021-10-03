import { Context } from '../../evaluator/interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface PrognSpecialExpressionNode extends SpecialExpressionNode {
  name: `progn`
}

export const prognSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const node: PrognSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `progn`,
      params: [],
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
  evaluate: (node, contextStack, evaluateAstNode) => {
    castPrognExpressionNode(node)
    const newContext: Context = {}

    const newContextStack = [newContext, ...contextStack]
    let result: unknown
    for (const form of node.params) {
      result = evaluateAstNode(form, newContextStack)
    }
    return result
  },
}

function castPrognExpressionNode(_node: SpecialExpressionNode): asserts _node is PrognSpecialExpressionNode {
  return
}
