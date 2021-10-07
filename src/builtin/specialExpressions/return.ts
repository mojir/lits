import { ReturnSignal } from '../../errors'
import { SpecialExpressionNode } from '../../parser/interface'
import { assertLength, asAstNode } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface ReturnSpecialExpressionNode extends SpecialExpressionNode {
  name: `return`
}

export const returnSpecialExpression: BuiltinSpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const node: ReturnSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `return`,
      params: [],
    }

    const [newPosition, valueNode] = parseToken(tokens, position)
    node.params.push(valueNode)
    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castReturnExpressionNode(node)
    const value = evaluateAstNode(asAstNode(node.params[0]), contextStack)
    throw new ReturnSignal(value)
  },
  validate: node => assertLength(1, node),
}

function castReturnExpressionNode(_node: SpecialExpressionNode): asserts _node is ReturnSpecialExpressionNode {
  return
}
