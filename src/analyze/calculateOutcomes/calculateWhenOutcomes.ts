import type { DoNode } from '../../builtin/specialExpressions/do'
import type { WhenNode } from '../../builtin/specialExpressions/when'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateWhenOutcomes: CalculatePossibleAstNodesHelper<WhenNode> = ({
  astNode,
  combinateAstNodes,
  nilNode,
  isAstComputable,
}) => {
  const condition = astNode.p[0]!

  if (isAstComputable(condition)) {
    return combinateAstNodes(astNode.p)
      .map(p => ({
        n: 'when',
        t: astNode.t,
        p,
        debugData: astNode.debugData,
      }))
  }

  const body = astNode.p.slice(1)

  return [
    ...combinateAstNodes(body)
      .map<DoNode>(p => ({
        n: 'do',
        t: astNode.t,
        p,
        debugData: astNode.debugData,
      })),
    nilNode,
  ]
}
