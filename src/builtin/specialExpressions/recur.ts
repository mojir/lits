import { AstNodeType } from '../../constants/constants'
import { RecurSignal } from '../../errors'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface RecurNode extends CommonSpecialExpressionNode<'recur'> {}

export const recurSpecialExpression: BuiltinSpecialExpression<null, RecurNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: RecurNode = {
      t: AstNodeType.SpecialExpression,
      n: 'recur',
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const params = node.p.map(paramNode => evaluateAstNode(paramNode, contextStack))
    throw new RecurSignal(params)
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
