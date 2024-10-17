import { AstNodeType, TokenType } from '../../constants/constants'
import { RecurSignal } from '../../errors'
import type { AstNode, CommonSpecialExpressionNode } from '../../parser/interface'
import { asToken } from '../../typeGuards/token'
import type { BuiltinSpecialExpression } from '../interface'

export interface RecurNode extends CommonSpecialExpressionNode<'recur'> {}

export const recurSpecialExpression: BuiltinSpecialExpression<null, RecurNode> = {
  parse: (tokenStream, position, firstToken, { parseTokensUntilClosingBracket }) => {
    let params: AstNode[]
    ;[position, params] = parseTokensUntilClosingBracket(tokenStream, position)
    const lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' })

    const node: RecurNode = {
      t: AstNodeType.SpecialExpression,
      n: 'recur',
      p: params,
      debugData: firstToken.debugData && {
        token: firstToken,
        lastToken,
      },
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const params = node.p.map(paramNode => evaluateAstNode(paramNode, contextStack))
    throw new RecurSignal(params)
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
