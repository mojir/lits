import { Any } from '../../interface'
import { AstNode, ParseToken, SpecialExpressionNode } from '../../parser/interface'
import { Token } from '../../tokenizer/interface'
import { token } from '../../utils/tokenAssertion'
import { BuiltinSpecialExpression } from '../interface'

export type Condition = {
  test: AstNode
  form: AstNode
}

type CondNode = SpecialExpressionNode & {
  conditions: Condition[]
}

function parseConditions(tokens: Token[], position: number, parseToken: ParseToken): [number, Condition[]] {
  const conditions: Condition[] = []

  let tkn = token.as(tokens[position], `EOF`)
  while (!token.is(tkn, { type: `paren`, value: `)` })) {
    let test: AstNode
    ;[position, test] = parseToken(tokens, position)

    let form: AstNode
    ;[position, form] = parseToken(tokens, position)

    conditions.push({ test, form })

    tkn = token.as(tokens[position], `EOF`)
  }
  return [position, conditions]
}

export const condSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseToken }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    let conditions: Condition[]
    ;[position, conditions] = parseConditions(tokens, position, parseToken)

    return [
      position + 1,
      {
        type: `SpecialExpression`,
        name: `cond`,
        conditions,
        params: [],
        token: firstToken.debugInfo ? firstToken : undefined,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    for (const condition of (node as CondNode).conditions) {
      const value = evaluateAstNode(condition.test, contextStack)
      if (!value) {
        continue
      }
      return evaluateAstNode(condition.form, contextStack)
    }
    return null
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const astNodes = (node as CondNode).conditions.flatMap(condition => [condition.test, condition.form])
    return analyzeAst(astNodes, contextStack, builtin)
  },
}
