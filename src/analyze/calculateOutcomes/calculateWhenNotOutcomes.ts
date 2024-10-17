import type { DoNode } from '../../builtin/specialExpressions/do'
import type { WhenNotNode } from '../../builtin/specialExpressions/when-not'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateWhenNotOutcomes: CalculatePossibleAstNodesHelper<WhenNotNode> = ({
  astNode,
  combinateAstNodes,
  nilNode,
  isAstComputable,
}) => {
  const condition = astNode.p[0]!

  if (isAstComputable(condition)) {
    return combinateAstNodes(astNode.p)
      .map<WhenNotNode>(p => ({
        n: 'when-not',
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
