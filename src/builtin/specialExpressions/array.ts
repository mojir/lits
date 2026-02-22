import { LitsError } from '../../errors'
import type { Any, Arr } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import { isSpreadNode } from '../../typeGuards/astNode'
import { asAny } from '../../typeGuards/lits'
import { chain, forEachSequential } from '../../utils/maybePromise'
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
  evaluate: (node, contextStack, { evaluateNode }) => {
    const result: Arr = []

    return chain(
      forEachSequential(node[1][1], (param) => {
        if (isSpreadNode(param)) {
          return chain(evaluateNode(param[1], contextStack), (spreadValue) => {
            if (!Array.isArray(spreadValue)) {
              throw new LitsError('Spread value is not an array', param[2])
            }
            result.push(...spreadValue)
          })
        }
        else {
          return chain(evaluateNode(param, contextStack), (value) => {
            result.push(value)
          })
        }
      }),
      () => result,
    )
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
