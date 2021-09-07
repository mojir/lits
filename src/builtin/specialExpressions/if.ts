import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNotUndefined, assertLengthThree } from '../../utils'
import { SpecialExpression } from '../interface'

interface IfSpecialExpressionNode extends SpecialExpressionNode {
  name: 'if'
}

export const ifSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseParams }) => {
    const { inputPosition } = asNotUndefined(tokens[position])
    const [newPosition, params] = parseParams(tokens, position)
    return [
      newPosition + 1,
      {
        type: 'SpecialExpression',
        name: 'if',
        params,
        inputPosition,
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castIfExpressionNode(node)

    const [conditionNode, trueNode, falseNode] = node.params
    const ifNode = evaluateAstNode(asAstNode(conditionNode), contextStack) ? asAstNode(trueNode) : asAstNode(falseNode)
    return evaluateAstNode(ifNode, contextStack)
  },
  validate: node => {
    assertLengthThree(node.params)
  },
}

function castIfExpressionNode(_node: SpecialExpressionNode): asserts _node is IfSpecialExpressionNode {
  return
}
