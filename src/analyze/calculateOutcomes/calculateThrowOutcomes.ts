import type { ThrowNode } from '../../builtin/specialExpressions/throw'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateThrowOutcomes: CalculatePossibleAstNodesHelper<ThrowNode> = ({
  astNode,
  calculatePossibleAstNodes,
}) => {
  return calculatePossibleAstNodes(astNode.p[0]!).map<ThrowNode>(m => ({
    ...astNode,
    p: [m],
  }))
}
