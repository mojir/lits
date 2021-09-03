import { Token } from '../..'
import { AstNode, ParseToken, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

type Condition = {
  test: AstNode
  body: AstNode[]
}

interface CondSpecialExpressionNode extends SpecialExpressionNode {
  name: 'cond'
  conditions: Condition[]
}

function parseConditions(tokens: Token[], position: number, parseToken: ParseToken): [number, Condition[]] {
  const conditions: Condition[] = []

  let token = asNotUndefined(tokens[position])
  while (!(token.type === 'paren' && token.value === ')')) {
    const [positionAfterTest, testNode] = parseToken(tokens, position + 1)
    position = positionAfterTest

    const body: AstNode[] = []
    token = asNotUndefined(tokens[position])
    while (!(token.type === 'paren' && token.value === ')')) {
      const [newPosition, node] = parseToken(tokens, position)
      body.push(node)
      position = newPosition
      token = asNotUndefined(tokens[position])
    }
    conditions.push({ test: testNode, body })
    position += 1
    token = asNotUndefined(tokens[position])
  }
  return [position, conditions]
}

export const condSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const [newPosition, conditions] = parseConditions(tokens, position, parseToken)
    return [
      newPosition + 1,
      {
        type: 'SpecialExpression',
        name: 'cond',
        conditions,
        params: [],
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    assertCondExpressionNode(node)
    let value: unknown

    for (const condition of node.conditions) {
      value = evaluateAstNode(condition.test, contextStack)
      if (!value) {
        continue
      }
      for (const bodyNode of condition.body) {
        value = evaluateAstNode(bodyNode, contextStack)
      }
      return value
    }
  },
  validate: node => {
    assertCondExpressionNode(node)
  },
}

function assertCondExpressionNode(node: SpecialExpressionNode): asserts node is CondSpecialExpressionNode {
  if (node.name !== 'cond') {
    throw Error('Expected cond special expression node')
  }
}
