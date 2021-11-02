import { Any } from '../../interface'
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

  let tkn = asNotUndefined(tokens[position], `EOF`)
  while (!(tkn.type === `paren` && tkn.value === `)`)) {
    let test: AstNode
    ;[position, test] = parseToken(tokens, position)

    let form: AstNode
    ;[position, form] = parseToken(tokens, position)

    conditions.push({ test, form })

    tkn = asNotUndefined(tokens[position], `EOF`)
  }
  return [position, conditions]
}

export const condSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseToken }) => {
    const firstToken = asNotUndefined(tokens[position], `EOF`)
    let conditions: Condition[]
    ;[position, conditions] = parseConditions(tokens, position, parseToken)

    return [
      position + 1,
      {
        type: `SpecialExpression`,
        name: `cond`,
        conditions,
        params: [],
        token: firstToken,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castCondExpressionNode(node)

    for (const condition of node.conditions) {
      const value = evaluateAstNode(condition.test, contextStack)
      if (!value) {
        continue
      }
      return evaluateAstNode(condition.form, contextStack)
    }
    return null
  },
}

function castCondExpressionNode(_node: SpecialExpressionNode): asserts _node is CondSpecialExpressionNode {
  return
}
