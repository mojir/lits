import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { isSymbolNode } from '../../typeGuards/astNode'
import { assertAny } from '../../typeGuards/lits'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonPolishSpecialExpressionParser } from './commonParser'

export interface QqNode extends CommonSpecialExpressionNode<'??'> {}

export const qqSpecialExpression: BuiltinSpecialExpression<Any, QqNode> = {
  polishParse: getCommonPolishSpecialExpressionParser('??'),
  validateParameterCount: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const [firstNode, secondNode] = node.p

    if (isSymbolNode(firstNode)) {
      if (contextStack.lookUp(firstNode) === null)
        return secondNode ? evaluateAstNode(secondNode, contextStack) : null
    }
    assertAny(firstNode, getTokenDebugData(node.token)?.sourceCodeInfo)
    const firstResult = evaluateAstNode(firstNode, contextStack)
    return firstResult ?? (secondNode ? evaluateAstNode(secondNode, contextStack) : null)
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
