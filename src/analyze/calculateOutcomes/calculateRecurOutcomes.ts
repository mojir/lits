import type { RecurNode } from '../../builtin/specialExpressions/recur'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateRecurOutcomes: CalculatePossibleAstNodesHelper<RecurNode> = ({
  astNode,
  combinateAstNodes,
}) => {
  return combinateAstNodes(astNode.p)
    .map(p => ({
      ...astNode,
      p,
    }))
}
