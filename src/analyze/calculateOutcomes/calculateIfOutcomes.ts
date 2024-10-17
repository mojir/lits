import type { IfNode } from '../../builtin/specialExpressions/if'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateIfOutcomes: CalculatePossibleAstNodesHelper<IfNode> = ({
  astNode,
  nilNode,
  calculatePossibleAstNodes,
  combinateAstNodes,
  isAstComputable,
}) => {
  const condition = astNode.p[0]!
  const thenBranch = astNode.p[1]!
  const elseBranch = astNode.p[2] ?? nilNode

  if (isAstComputable(condition)) {
    return combinateAstNodes(astNode.p)
      .map(p => ({
        n: 'if',
        t: astNode.t,
        p,
        debugData: astNode.debugData,
      }))
  }

  return [
    ...calculatePossibleAstNodes(thenBranch),
    ...calculatePossibleAstNodes(elseBranch),
  ]
}
