import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, assertLengthThree } from '../../utils'
import { SpecialExpression } from '../interface'

interface IfSpecialExpressionNode extends SpecialExpressionNode {
  name: 'if'
}

export const ifSpecialExpression: SpecialExpression = {
  parse: (_tokens, position) => {
    return [
      position,
      {
        type: 'SpecialExpression',
        name: 'if',
        params: [],
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    assertIfExpressionNode(node)

    const [conditionNode, trueNode, falseNode] = node.params
    const ifNode = evaluateAstNode(asAstNode(conditionNode), contextStack) ? asAstNode(trueNode) : asAstNode(falseNode)
    return evaluateAstNode(ifNode, contextStack)
  },
  validate: node => {
    assertIfExpressionNode(node)
    assertLengthThree(node.params)
  },
}

function assertIfExpressionNode(node: SpecialExpressionNode): asserts node is IfSpecialExpressionNode {
  if (node.name !== 'if') {
    throw Error('Expected if special expression node')
  }
}
