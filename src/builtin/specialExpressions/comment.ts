import type { CommonSpecialExpressionNode } from '../../parser/interface'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonPolishSpecialExpressionParser } from './commonParser'

export interface CommentExpressionNode extends CommonSpecialExpressionNode<'comment'> {}

export const commentSpecialExpression: BuiltinSpecialExpression<null, CommentExpressionNode> = {
  polishParse: getCommonPolishSpecialExpressionParser('comment'),
  validateParameterCount: () => undefined,
  evaluate: () => null,
  findUnresolvedIdentifiers: () => new Set(),
}
