import type { TryNode } from '../../builtin/specialExpressions/try'
import { asNameNode } from '../../typeGuards/astNode'
import { combinate } from '../utils'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateTryOutcomes: CalculatePossibleAstNodesHelper<TryNode> = ({
  astNode,
  calculatePossibleAstNodes,
}) => {
  return combinate([
    calculatePossibleAstNodes(astNode.p[0]!),
    calculatePossibleAstNodes(astNode.ce),
    calculatePossibleAstNodes(astNode.e),
  ],
  )
    .map<TryNode>(combination => ({
      ...astNode,
      te: combination[0]!,
      ce: combination[1]!,
      e: asNameNode(combination[2]),
    }))
}
