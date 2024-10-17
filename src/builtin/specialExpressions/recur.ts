import { RecurSignal } from '../../errors'
import { AstNodeType } from '../../constants/constants'
import type { SpecialExpressionNode } from '../../parser/interface'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export const recurSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokenStream, position, { parseTokens }) => {
    const firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath)
    let params
    ;[position, params] = parseTokens(tokenStream, position)

    const node: SpecialExpressionNode = {
      t: AstNodeType.SpecialExpression,
      n: 'recur',
      p: params,
      tkn: firstToken.sourceCodeInfo ? firstToken : undefined,
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const params = node.p.map(paramNode => evaluateAstNode(paramNode, contextStack))
    throw new RecurSignal(params)
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyzeAst(node.p, contextStack, builtin),
}
