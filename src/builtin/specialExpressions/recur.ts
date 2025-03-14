import { AstNodeType } from '../../constants/constants'
import { RecurSignal } from '../../errors'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/tokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface RecurNode extends CommonSpecialExpressionNode<'recur'> {}

export const recurSpecialExpression: BuiltinSpecialExpression<null, RecurNode> = {
  polishParse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
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
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const params = node.p.map(paramNode => evaluateAstNode(paramNode, contextStack))
    throw new RecurSignal(params)
  },
  findUnresolvedSymbols: (node, contextStack, { findUnresolvedSymbols, builtin }) => findUnresolvedSymbols(node.p, contextStack, builtin),
}
