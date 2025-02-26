import type { Any } from '../../interface'
import type { CommonSpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams } from '../../typeGuards'
import type { BuiltinSpecialExpression } from '../interface'
import { getCommonParser } from './commonParser'

export interface TimeNode extends CommonSpecialExpressionNode<'time!'> {}

export const timeSpecialExpression: BuiltinSpecialExpression<Any, TimeNode> = {
  parse: getCommonParser('time!'),
  validateParameterCount: node => assertNumberOfParams(1, node),
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const param = node.p[0]!
    const startTime = Date.now()
    const result = evaluateAstNode(param, contextStack)
    const totalTime = Date.now() - startTime
    // eslint-disable-next-line no-console
    console.log(`Elapsed time: ${totalTime} ms`)

    return result
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => findUnresolvedIdentifiers(node.p, contextStack, builtin),
}
