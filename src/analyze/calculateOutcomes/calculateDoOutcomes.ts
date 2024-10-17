import type { DoNode } from '../../builtin/specialExpressions/do'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateDoOutcomes: CalculatePossibleAstNodesHelper<DoNode> = ({
  astNode,
  combinateAstNodes,
}) => {
  return combinateAstNodes(astNode.p).map<DoNode>(p => ({
    ...astNode,
    p,
  }))
}
