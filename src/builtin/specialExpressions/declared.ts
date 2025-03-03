import type { CommonSpecialExpressionNode, SymbolNode } from '../../parser/interface'
import { assertNumberOfParams } from '../../typeGuards'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonPolishSpecialExpressionParser } from './commonParser'

export interface DeclaredNode extends CommonSpecialExpressionNode<'defined?'> {}

export const declaredSpecialExpression: BuiltinSpecialExpression<boolean, DeclaredNode> = {
  polishParse: getCommonPolishSpecialExpressionParser('defined?'),
  validateParameterCount: node => assertNumberOfParams(1, node),
  evaluate: (node, contextStack) => {
    const lookUpResult = contextStack.lookUp(node.p[0] as SymbolNode)
    return lookUpResult !== null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
