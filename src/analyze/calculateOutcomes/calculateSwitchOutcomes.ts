import type { SwitchNode } from '../../builtin/specialExpressions/switch'
import { arrayToPairs } from '../../utils'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateSwitchOutcomes: CalculatePossibleAstNodesHelper<SwitchNode> = ({
  astNode,
  nilNode,
  calculatePossibleAstNodes,
}) => {
  // TODO be smarter about this
  return [
    ...arrayToPairs(astNode.p.slice(1)).flatMap(([_, form]) => calculatePossibleAstNodes(form!)),
    nilNode,
  ]
}
