import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { getTokenDebugData } from '../../tokenizer/utils'
import { assertNumberOfParams } from '../../typeGuards'
import { asAstNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonPolishSpecialExpressionParser } from './commonParser'

export interface IfNode extends CommonSpecialExpressionNode<'if'> {}

export const ifSpecialExpression: BuiltinSpecialExpression<Any, IfNode> = {
  polishParse: getCommonPolishSpecialExpressionParser('if'),
  validateParameterCount: node => assertNumberOfParams({ min: 2, max: 3 }, node),
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo

    const [conditionNode, trueNode, falseNode] = node.p
    if (evaluateAstNode(asAstNode(conditionNode, sourceCodeInfo), contextStack)) {
      return evaluateAstNode(asAstNode(trueNode, sourceCodeInfo), contextStack)
    }
    else {
      if (node.p.length === 3)
        return evaluateAstNode(asAstNode(falseNode, sourceCodeInfo), contextStack)
      else
        return null
    }
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
