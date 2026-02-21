import type { Any } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import { asAny } from '../../typeGuards/lits'
import type { BuiltinSpecialExpression, FunctionDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type OrNode = SpecialExpressionNode<[typeof specialExpressionTypes['||'], AstNode[]]>

const docs: FunctionDocs = {
  category: 'special-expression',
  returns: {
    type: 'boolean',
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
    { argumentNames: ['a', 'b'] },
    { argumentNames: ['a', 'b', 'c'] },
  ],
  description: `
  Computes logical \`or\`. Evaluation of expressions evaluation starts from left.
  As soon as a \`expression\` evaluates to a truthy value, the result is returned.

  If all expressions evaluate to falsy values, the value of the last expression is returned.`,
  examples: [
    'false || 1',
    '||(1, 1)',
    '||(3 > 2, "string")',
    '||(3 < 2, "string")',
    '||(false, false, false, true)',
    '||(1, 2, 3, 4)',
  ],
}

export const orSpecialExpression: BuiltinSpecialExpression<Any, OrNode> = {
  arity: {},
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    let value: Any = false

    for (const param of node[1][1]) {
      value = evaluateNode(param, contextStack)
      if (value)
        break
    }

    return value
  },
  evaluateAsNormalExpression: (params, sourceCodeInfo) => {
    let value: Any = false
    for (const param of params) {
      value = asAny(param, sourceCodeInfo)
      if (value)
        break
    }
    return value
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
