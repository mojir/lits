import type { DoSeqNode, ForNode } from '../../builtin/specialExpressions/loops'
import type { AstNode } from '../../parser/interface'
import type { CalculatePossibleAstNodes, CalculatePossibleAstNodesHelper } from '.'

export const calculateForOutcomes: CalculatePossibleAstNodesHelper<ForNode> = ({
  astNode,
  calculatePossibleAstNodes,
}) => {
  if (!isDeterministic(calculatePossibleAstNodes, astNode))
    throw new Error('Could not calculate for loop, not deterministic')

  return [astNode]
}

export const calculateDoSeqOutcomes: CalculatePossibleAstNodesHelper<DoSeqNode> = ({
  astNode,
  calculatePossibleAstNodes,
}) => {
  if (!isDeterministic(calculatePossibleAstNodes, astNode))
    throw new Error('Could not calculate doSeq node, not deterministic')

  return [astNode]
}

function isDeterministic(calculatePossibleAstNodes: CalculatePossibleAstNodes, astNode: ForNode | DoSeqNode): boolean {
  for (const { b, l, wn, we } of astNode.l) {
    if (l && l.some(({ v }) => !astIsDeterministic(calculatePossibleAstNodes, v)))
      return false

    if (!astIsDeterministic(calculatePossibleAstNodes, b.v))
      return false

    if (wn && !astIsDeterministic(calculatePossibleAstNodes, wn))
      return false

    if (we && !astIsDeterministic(calculatePossibleAstNodes, we))
      return false
  }

  if (!astIsDeterministic(calculatePossibleAstNodes, astNode.p[0]!))
    return false

  return true
}

function astIsDeterministic(calculatePossibleAstNodes: CalculatePossibleAstNodes, astNode: AstNode) {
  return calculatePossibleAstNodes(astNode).length === 1
}
