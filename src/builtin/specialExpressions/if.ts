import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, assertLength } from '../../utils'
import { SpecialExpression } from '../interface'

interface IfSpecialExpressionNode extends SpecialExpressionNode {
  name: `if`
}

export const ifSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseTokens }) => {
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `if`,
        params,
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castIfExpressionNode(node)

    const [conditionNode, trueNode, falseNode] = node.params
    const ifNode = evaluateAstNode(asAstNode(conditionNode), contextStack) ? asAstNode(trueNode) : asAstNode(falseNode)
    return evaluateAstNode(ifNode, contextStack)
  },
  validate: node => assertLength(3, node),
}

function castIfExpressionNode(_node: SpecialExpressionNode): asserts _node is IfSpecialExpressionNode {
  return
}
