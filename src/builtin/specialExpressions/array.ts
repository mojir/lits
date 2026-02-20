import { LitsError } from '../../errors'
import type { Any, Arr } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import { isSpreadNode } from '../../typeGuards/astNode'
import { asAny } from '../../typeGuards/lits'
import type { BuiltinSpecialExpression, FunctionDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ArrayNode = SpecialExpressionNode<[typeof specialExpressionTypes['array'], Node[]]>

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
  evaluate: (node, contextStack, { evaluateNode }) => {
    const result: Arr = []

    for (const param of node[1][1]) {
      if (isSpreadNode(param)) {
        const spreadValue = evaluateNode(param[1], contextStack)
        if (!Array.isArray(spreadValue)) {
          throw new LitsError('Spread value is not an array', param[2])
        }
        result.push(...spreadValue)
      }
      else {
        result.push(evaluateNode(param, contextStack))
      }
    }

    return result
  },
  evaluateAsNormalExpression: (params, sourceCodeInfo) => {
    const result: Arr = []

    for (const param of params) {
      result.push(asAny(param, sourceCodeInfo))
    }

    return result
  },

  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
