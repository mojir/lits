import type { IfNotNode } from '../../builtin/specialExpressions/if_not'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateIfNotOutcomes: CalculatePossibleAstNodesHelper<IfNotNode> = ({
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
        n: 'if_not',
        t: astNode.t,
        p,
        token: astNode.token,
      }))
  }

  return [
    ...calculatePossibleAstNodes(thenBranch),
    ...calculatePossibleAstNodes(elseBranch),
  ]
}
