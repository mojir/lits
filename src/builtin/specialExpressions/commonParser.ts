import type { CommonSpecialExpressionName, GenericCommonSpecialExpressionNode } from '..'
import { AstNodeType } from '../../constants/constants'
import type { Any } from '../../interface'
import { assertRParenToken } from '../../tokenizer/tokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import type { BuiltinSpecialExpression } from '../interface'

export function getCommonPolishSpecialExpressionParser<T extends CommonSpecialExpressionName>(name: T): BuiltinSpecialExpression<Any, GenericCommonSpecialExpressionNode<T>>['polishParse'] {
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
