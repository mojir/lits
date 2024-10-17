import type { LoopNode } from '../../builtin/specialExpressions/loop'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateLoopOutcomes: CalculatePossibleAstNodesHelper<LoopNode> = ({
  astNode,
  combinateAstNodes,
}) => {
  return combinateAstNodes(astNode.p, astNode.bs.map(bindingNode => bindingNode.n))
    .map<LoopNode>(p => ({
      ...astNode,
      p,
    }))
}
