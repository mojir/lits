import { AstNodeType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { asRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface OrNode extends CommonSpecialExpressionNode<'or'> {}

export const orSpecialExpression: BuiltinSpecialExpression<Any, OrNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: OrNode = {
      t: AstNodeType.SpecialExpression,
      n: 'or',
      p: params,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    let value: Any = false

    for (const param of node.p) {
      value = evaluateAstNode(param, contextStack)
      if (value)
        break
    }

    return value
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
