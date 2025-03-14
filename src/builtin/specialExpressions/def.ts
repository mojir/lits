import type { CommonSpecialExpressionNode, SymbolNode } from '../../parser/interface'
import { getTokenDebugData } from '../../tokenizer/token'
import { asAstNode, asSymbolNode } from '../../typeGuards/astNode'
import type { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

export interface DefNode extends CommonSpecialExpressionNode<'def'> {}

export const defSpecialExpression: BuiltinSpecialExpression<null, DefNode> = {
  paramCount: 2,
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo
    const name = (node.p[0] as SymbolNode).v

    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)

    contextStack.exportValue(name, evaluateAstNode(node.p[1]!, contextStack))

    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin }) => {
    const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo
    const subNode = asAstNode(node.p[1])
    const result = getUndefinedSymbols([subNode], contextStack, builtin)
    const name = asSymbolNode(node.p[0]).v
    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)
    contextStack.exportValue(name, true)
    return result
  },
}
