import { AstNodeType } from '../../constants/constants'
import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { asRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { assertAstNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'

export interface WhenNode extends CommonSpecialExpressionNode<'when'> {}

export const whenSpecialExpression: BuiltinSpecialExpression<Any, WhenNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    const lastToken = asRParenToken(tokenStream.tokens[parseState.position++])

    const node: WhenNode = {
      t: AstNodeType.SpecialExpression,
      n: 'when',
      p: params,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
        lastToken,
      },
    }

    assertNumberOfParams({ min: 1 }, node)

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const [whenExpression, ...body] = node.p
    assertAstNode(whenExpression, getTokenDebugData(node.debugData?.token)?.sourceCodeInfo)

    if (!evaluateAstNode(whenExpression, contextStack))
      return null

    let result: Any = null
    for (const form of body)
      result = evaluateAstNode(form, contextStack)

    return result
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
