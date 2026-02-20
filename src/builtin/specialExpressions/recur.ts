import { RecurSignal } from '../../errors'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type RecurNode = SpecialExpressionNode<[typeof specialExpressionTypes['recur'], Node[]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['recur(...recur-args)'],
  description: 'Recursevly calls enclosing function or loop with its evaluated `recur-args`.',
  examples: [
    `
let foo = (n) -> do
  write!(n);
  if !(zero?(n)) then
    recur(n - 1)
  end
end;
foo(3)`,
    `
(n -> do
  write!(n);
  if !(zero?(n)) then
    recur(n - 1)
  end
end)(3)`,
    `
loop (n = 3) -> do
  write!(n);
  if !(zero?(n)) then
    recur(n - 1)
  end
end`,
  ],
}

export const recurSpecialExpression: BuiltinSpecialExpression<null, RecurNode> = {
  arity: {},
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const params = node[1][1]
    const evaluatedParams = params.map(paramNode => evaluateNode(paramNode, contextStack))
    throw new RecurSignal(evaluatedParams)
  },
  evaluateAsNormalExpression: (params) => {
    throw new RecurSignal(params)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) =>
    getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode),
}
