import { ReturnSignal } from '../../errors'
import { SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLength } from '../../utils'
import { SpecialExpression } from '../interface'

interface ReturnSpecialExpressionNode extends SpecialExpressionNode {
  name: 'return'
}

export const returnSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const node: ReturnSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'return',
      params: [],
    }

    const [newPosition, valueNode] = parseToken(tokens, position)
    node.params.push(valueNode)
    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castReturnExpressionNode(node)
    const value = evaluateAstNode(asNotUndefined(node.params[0]), contextStack)
    throw new ReturnSignal(value)
  },
  validate: node => assertLength(1, node.params),
}

function castReturnExpressionNode(_node: SpecialExpressionNode): asserts _node is ReturnSpecialExpressionNode {
  return
}
