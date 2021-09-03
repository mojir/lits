import { SpecialExpressionNode } from '../../parser/interface'
import { SpecialExpression } from '../interface'

interface AndSpecialExpressionNode extends SpecialExpressionNode {
  name: 'and'
}

export const andSpecialExpression: SpecialExpression = {
  parse: (_tokens, position) => {
    return [
      position,
      {
        type: 'SpecialExpression',
        name: 'and',
        params: [],
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    assertAndExpressionNode(node)
    let value: unknown = true

    for (const param of node.params) {
      value = evaluateAstNode(param, contextStack)
      if (!value) {
        break
      }
    }

    return value
  },
  validate: node => {
    assertAndExpressionNode(node)
  },
}

function assertAndExpressionNode(node: SpecialExpressionNode): asserts node is AndSpecialExpressionNode {
  if (node.name !== 'and') {
    throw Error('Expected and special expression node')
  }
}
