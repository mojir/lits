import { AstNode, ParseToken, SpecialExpressionNode } from '../../parser/interface'
import { Token } from '../../tokenizer/interface'
import { asNotUndefined } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

type Condition = {
  test: AstNode
  form: AstNode
}

interface CondSpecialExpressionNode extends SpecialExpressionNode {
  name: `cond`
  conditions: Condition[]
}

function parseConditions(tokens: Token[], position: number, parseToken: ParseToken): [number, Condition[]] {
  const conditions: Condition[] = []

  let token = asNotUndefined(tokens[position])
  while (!(token.type === `paren` && token.value === `)`)) {
    let test: AstNode
    ;[position, test] = parseToken(tokens, position)

    let form: AstNode
    ;[position, form] = parseToken(tokens, position)

    conditions.push({ test, form })

    token = asNotUndefined(tokens[position])
  }
  return [position, conditions]
}

export const condSpecialExpression: BuiltinSpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    let conditions: Condition[]
    ;[position, conditions] = parseConditions(tokens, position, parseToken)

    return [
      position + 1,
      {
        type: `SpecialExpression`,
        name: `cond`,
        conditions,
        params: [],
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castCondExpressionNode(node)
    let value: unknown

    for (const condition of node.conditions) {
      value = evaluateAstNode(condition.test, contextStack)
      if (!value) {
        continue
      }
      return evaluateAstNode(condition.form, contextStack)
    }
  },
}

function castCondExpressionNode(_node: SpecialExpressionNode): asserts _node is CondSpecialExpressionNode {
  return
}
