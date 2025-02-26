import type { CommonSpecialExpressionNode } from '../../parser/interface'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonParser } from './commonParser'

export interface CommentExpressionNode extends CommonSpecialExpressionNode<'comment'> {}

export const commentSpecialExpression: BuiltinSpecialExpression<null, CommentExpressionNode> = {
  parse: getCommonParser('comment'),
  validateParameterCount: () => undefined,
  evaluate: () => null,
  findUnresolvedIdentifiers: () => new Set(),
}
