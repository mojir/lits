import { SpecialExpressionNode } from '../../parser/interface'
import { SpecialExpression } from '../interface'

interface OrSpecialExpressionNode extends SpecialExpressionNode {
  name: 'and'
}

export const orSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
    return [
      newPosition + 1,
      {
        type: 'SpecialExpression',
        name: 'or',
        params,
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    assertOrExpressionNode(node)
    let value: unknown = false

    for (const param of node.params) {
      value = evaluateAstNode(param, contextStack)
      if (value) {
        break
      }
    }

    return value
  },
  validate: node => {
    assertOrExpressionNode(node)
  },
}

function assertOrExpressionNode(node: SpecialExpressionNode): asserts node is OrSpecialExpressionNode {
  if (node.name !== 'or') {
    throw Error('Expected or special expression node')
  }
}
