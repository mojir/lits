import { AstNodeType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/tokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import type { BuiltinSpecialExpression } from '../interface'

export interface AndNode extends CommonSpecialExpressionNode<'&&'> {}

export const andSpecialExpression: BuiltinSpecialExpression<Any, AndNode> = {
  polishParse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: AndNode = {
      t: AstNodeType.SpecialExpression,
      n: '&&',
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  },
  paramCount: {},
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    let value: Any = true

    for (const param of node.p) {
      value = evaluateAstNode(param, contextStack)
      if (!value)
        break
    }

    return value
  },
  findUnresolvedSymbols: (node, contextStack, { findUnresolvedSymbols, builtin }) => findUnresolvedSymbols(node.p, contextStack, builtin),
}
