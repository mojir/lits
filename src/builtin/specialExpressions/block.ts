import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import { reduceSequential } from '../../utils/maybePromise'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type DoNode = SpecialExpressionNode<[typeof specialExpressionTypes['block'], AstNode[]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['do body end'],
  details: [
    ['body', 'expressions', 'The expressions to evaluate.'],
  ],
  description: 'Evaluates `body`. Resulting value is the value of the last expression.',
  examples: [
    `
do
  let a = 1 + 2 + 3 + 4;
  let b = -> $ * ( $ + 1 );
  b(a)
end`,
  ],
}

export const doSpecialExpression: BuiltinSpecialExpression<Any, DoNode> = {
  arity: {},
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const newContext: Context = {}

    const newContextStack = contextStack.create(newContext)
    return reduceSequential(
      node[1][1],
      (_acc, form) => evaluateNode(form, newContextStack),
      null as Any,
    )
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    return getUndefinedSymbols(node[1][1], contextStack.create({}), builtin, evaluateNode)
  },
}
