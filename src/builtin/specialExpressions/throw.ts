import { AstNodeType } from '../../constants/constants'
import { UserDefinedError } from '../../errors'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { asString } from '../../typeGuards/string'
import type { BuiltinSpecialExpression } from '../interface'

export interface ThrowNode extends CommonSpecialExpressionNode<'throw'> {}

export const throwSpecialExpression: BuiltinSpecialExpression<null, ThrowNode> = {
  parse: (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)

    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: ThrowNode = {
      t: AstNodeType.SpecialExpression,
      n: 'throw',
      p: params,
      debugData: getTokenDebugData(firstToken) && {
        token: firstToken,
      },
    }

    assertNumberOfParams(1, node)

    return node
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const message = asString(evaluateAstNode(node.p[0]!, contextStack), getTokenDebugData(node.debugData?.token)?.sourceCodeInfo, {
      nonEmpty: true,
    })
    throw new UserDefinedError(message, getTokenDebugData(node.debugData?.token)?.sourceCodeInfo)
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
