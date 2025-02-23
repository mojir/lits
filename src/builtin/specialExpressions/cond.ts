import { AstNodeType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertEvenNumberOfParams } from '../../typeGuards'
import { arrayToPairs } from '../../utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface CondNode extends CommonSpecialExpressionNode<'cond'> {}

export const condSpecialExpression: BuiltinSpecialExpression<Any, CondNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: CondNode = {
      t: AstNodeType.SpecialExpression,
      n: 'cond',
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    assertEvenNumberOfParams(node)

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    for (const [test, form] of arrayToPairs(node.p)) {
      const value = evaluateAstNode(test!, contextStack)
      if (!value)
        continue

      return evaluateAstNode(form!, contextStack)
    }
    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
