import { RecurSignal } from '../../errors'
import { SpecialExpressionNode } from '../../parser/interface'
import { token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

interface RecurSpecialExpressionNode extends SpecialExpressionNode {
  name: `recur`
}

export const recurSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    let params
    ;[position, params] = parseTokens(tokens, position)

    const node: RecurSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `recur`,
      params,
      token: firstToken,
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castReturnExpressionNode(node)
    const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
    throw new RecurSignal(params)
  },
}

function castReturnExpressionNode(_node: SpecialExpressionNode): asserts _node is RecurSpecialExpressionNode {
  return
}
