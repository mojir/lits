import { RecurSignal } from '../../errors'
import { SpecialExpressionNode } from '../../parser/interface'
import { assertLength } from '../../utils'
import { SpecialExpression } from '../interface'

interface RecurSpecialExpressionNode extends SpecialExpressionNode {
  name: `recur`
}

export const recurSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseTokens }) => {
    let params
    ;[position, params] = parseTokens(tokens, position)

    const node: RecurSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `recur`,
      params,
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castReturnExpressionNode(node)
    const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
    throw new RecurSignal(params)
  },
  validate: node => assertLength(1, node),
}

function castReturnExpressionNode(_node: SpecialExpressionNode): asserts _node is RecurSpecialExpressionNode {
  return
}
