import type { DefNode } from '../../builtin/specialExpressions/def'
import { asSymbolNode } from '../../typeGuards/astNode'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateDefOutcomes: CalculatePossibleAstNodesHelper<DefNode> = ({
  astNode,
  calculatePossibleAstNodes,
  addGlobalIdentifier,
}) => {
  const nameNode = asSymbolNode(astNode.p[0])
  const valueNode = astNode.p[1]!
  addGlobalIdentifier(nameNode.v)
  return calculatePossibleAstNodes(valueNode)
    .map<DefNode>(node => ({
      ...astNode,
      p: [nameNode, node],
    }))
}
