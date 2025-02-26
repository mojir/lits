import type { CommonSpecialExpressionName, GenericCommonSpecialExpressionNode } from '..'
import { AstNodeType } from '../../constants/constants'
import type { Any } from '../../interface'
import { assertRParenToken } from '../../tokenizer/common/commonTokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import type { BuiltinSpecialExpression } from '../interface'

export function getCommonParser<T extends CommonSpecialExpressionName>(name: T): BuiltinSpecialExpression<Any, GenericCommonSpecialExpressionNode<T>>['parse'] {
  return (tokenStream, parseState, firstToken, { parseTokensUntilClosingBracket }) => {
    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    return {
      t: AstNodeType.SpecialExpression,
      n: name,
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    } as unknown as GenericCommonSpecialExpressionNode<T>
  }
}
