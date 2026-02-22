import type { Any } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import { isUserDefinedSymbolNode } from '../../typeGuards/astNode'
import { asAny } from '../../typeGuards/lits'
import { chain, reduceSequential } from '../../utils/maybePromise'
import type { BuiltinSpecialExpression, FunctionDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type QqNode = SpecialExpressionNode<[typeof specialExpressionTypes['??'], AstNode[]]>

const docs: FunctionDocs = {
  category: 'special-expression',
  returns: {
    type: 'any',
  },
  args: {
    a: { type: 'any' },
    b: { type: 'any' },
    c: {
      type: 'any',
      rest: true,
    },
  },
  variants: [
    { argumentNames: ['a'] },
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'c'] },
  ],
  description: `Nullish coalescing operator. Returns the first non-\`null\` value.

Evaluation is short-circuited — as soon as a non-\`null\` value is found, the remaining expressions are not evaluated.

If all values are \`null\`, returns \`null\`.

Also works with undefined symbols — if a symbol is undefined, it is treated as \`null\`.`,
  examples: [
    '1 ?? 2',
    'null ?? 2',
    '??(null)',
    '??(null, "default")',
    '??(1, "default")',
    'false ?? "default"',
    '??(null, null, 3)',
  ],
}

export const qqSpecialExpression: BuiltinSpecialExpression<Any, QqNode> = {
  arity: { min: 1 },
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    // Use a sentinel to know we haven't found a non-null value yet
    const SENTINEL = Symbol('qq-sentinel')
    type SentinelOrAny = Any | typeof SENTINEL
    return chain(
      reduceSequential(
        node[1][1],
        (acc: SentinelOrAny, param) => {
          if (acc !== SENTINEL)
            return acc
          if (isUserDefinedSymbolNode(param) && contextStack.lookUp(param) === null) {
            return SENTINEL
          }
          return chain(evaluateNode(param, contextStack), (result) => {
            if (result !== null) {
              return result
            }
            return SENTINEL
          })
        },
        SENTINEL as SentinelOrAny,
      ),
      result => result === SENTINEL ? null : result as Any,
    )
  },

  evaluateAsNormalExpression: (params, sourceCodeInfo) => {
    for (const param of params) {
      const value = asAny(param, sourceCodeInfo)
      if (value !== null) {
        return value
      }
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
