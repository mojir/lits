import { RecurSignal } from '../../errors'
import { SpecialExpressionNode } from '../../parser/interface'
import { token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

export const recurSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    let params
    ;[position, params] = parseTokens(tokens, position)

    const node: SpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `recur`,
      params,
      token: firstToken.debugInfo ? firstToken : undefined,
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const params = node.params.map(paramNode => evaluateAstNode(paramNode, contextStack))
    throw new RecurSignal(params)
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyzeAst(node.params, contextStack, builtin),
}
