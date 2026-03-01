import type { Any } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import { joinSets } from '../../utils'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type RaceNode = SpecialExpressionNode<[typeof specialExpressionTypes['race'], AstNode[]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: [
    'race(expr1, expr2, ...)',
  ],
  details: [
    ['expr1, expr2, ...', 'expressions', 'Expressions to race concurrently. Typically `perform(...)` calls.'],
  ],
  description: 'Races all branch expressions concurrently. The first branch to complete wins — '
    + 'its value becomes the result. Losing branches are cancelled via AbortSignal. '
    + 'Errored branches are silently dropped. If all branches error, throws an aggregate error. '
    + 'If no branch completes but some suspend, the race suspends. On resume, the host provides '
    + 'the winner value directly. Only available in async mode (`run()`). Requires at least one branch.',
  examples: [],
}

export const raceSpecialExpression: BuiltinSpecialExpression<Any, RaceNode> = {
  arity: { min: 1 },
  docs,
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const branches = node[1][1] as AstNode[]
    const sets = branches.map(branch => getUndefinedSymbols([branch], contextStack, builtin, evaluateNode))
    return joinSets(...sets)
  },
}
