import type { Arr } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import { joinSets } from '../../utils'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type ParallelNode = SpecialExpressionNode<[typeof specialExpressionTypes['parallel'], AstNode[]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: [
    'parallel(expr1, expr2, ...)',
  ],
  details: [
    ['expr1, expr2, ...', 'expressions', 'Expressions to evaluate concurrently. Typically `perform(...)` calls.'],
  ],
  description: 'Evaluates all branch expressions concurrently and returns an array of results in order. '
    + 'Each branch runs as an independent trampoline invocation. If any branch suspends, '
    + 'the entire `parallel` suspends with a composite blob. On resume, branches are resumed '
    + 'one at a time. Only available in async mode (`run()`). Requires at least one branch.',
  examples: [],
}

export const parallelSpecialExpression: BuiltinSpecialExpression<Arr, ParallelNode> = {
  arity: { min: 1 },
  docs,
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const branches = node[1][1] as AstNode[]
    const sets = branches.map(branch => getUndefinedSymbols([branch], contextStack, builtin, evaluateNode))
    return joinSets(...sets)
  },
}
