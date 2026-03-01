import type { Any, Arr } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import { asAny } from '../../typeGuards/lits'
import type { BuiltinSpecialExpression, FunctionDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ArrayNode = SpecialExpressionNode<[typeof specialExpressionTypes['array'], AstNode[]]>

const docs: FunctionDocs = {
  category: 'special-expression',
  returns: {
    type: 'any',
    array: true,
  },
  args: {
    values: {
      type: 'any',
      rest: true,
    },
  },
  variants: [
    { argumentNames: ['values'] },
  ],
  description: 'Makes new array from $values.',
  examples: [
    'array(1, 2, 3)',
    'array(array(null, false, true))',
    '[]',
    '[1, 2, 3]',
    '[1, 2, ...[3, 4, 5], 6]',
    '[[null, false, true]]',
    '[1, 2, 3][1]',
  ],
  hideOperatorForm: true,
}

export const arraySpecialExpression: BuiltinSpecialExpression<Any, ArrayNode> = {
  arity: {},
  docs,
  evaluateAsNormalExpression: (params, sourceCodeInfo) => {
    const result: Arr = []

    for (const param of params) {
      result.push(asAny(param, sourceCodeInfo))
    }

    return result
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
