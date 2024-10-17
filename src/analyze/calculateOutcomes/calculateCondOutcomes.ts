import type { CondNode } from '../../builtin/specialExpressions/cond'
import type { AstNode } from '../../parser/interface'
import { combinate } from '../utils'
import { arrayToPairs } from '../../utils'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateCondOutcomes: CalculatePossibleAstNodesHelper<CondNode> = ({
  astNode,
  nilNode,
  calculatePossibleAstNodes,
  isAstComputable,
}) => {
  const testNodes = arrayToPairs(astNode.p).map(([t]) => t!)
  if (isAstComputable(testNodes)) {
    return combinate(arrayToPairs(astNode.p)
      // Create a list of ast nodes from the test and form of each condition
      .reduce((acc: AstNode[][], [test, form]) => {
        acc.push(calculatePossibleAstNodes(test!), calculatePossibleAstNodes(form!))
        return acc
      }, []),
    )
      // Create a new CondNode for each combination of test and form outcomes
      .map<CondNode>(conditionAsts => ({
        ...astNode,
        c: arrayToPairs(conditionAsts).map(([t, f]) => ({ t: t!, f: f! })),
      }))
  }

  return [
    ...arrayToPairs(astNode.p).flatMap(([_, form]) => calculatePossibleAstNodes(form!)),
    nilNode,
  ]
}
