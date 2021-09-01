import { evaluateAstNode } from '../../evaluator'
import { Context } from '../../evaluator/interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { Token } from '../../tokenizer/interface'
import { asAstNode, assertLengthThree } from '../../utils'
import { SpecialExpression } from '../interface'

interface IfSpecialExpressionNode extends SpecialExpressionNode {
  name: 'if'
}

export const ifSpecialExpression: SpecialExpression = {
  parse: (_tokens: Token[], position: number) => {
    return [
      position,
      {
        type: 'SpecialExpression',
        name: 'if',
        params: [],
      },
    ]
  },
  evaluate: (node: SpecialExpressionNode, contextStack: Context[]) => {
    assertIfExpressionNode(node)

    const [conditionNode, trueNode, falseNode] = node.params
    const ifNode = evaluateAstNode(asAstNode(conditionNode), contextStack) ? asAstNode(trueNode) : asAstNode(falseNode)
    return evaluateAstNode(ifNode, contextStack)
  },
  validate: (node: SpecialExpressionNode) => {
    assertIfExpressionNode(node)
    assertLengthThree(node.params)
  },
}

function assertIfExpressionNode(node: SpecialExpressionNode): asserts node is IfSpecialExpressionNode {
  if (node.name !== 'if') {
    throw Error('Expected if special expression node')
  }
}
