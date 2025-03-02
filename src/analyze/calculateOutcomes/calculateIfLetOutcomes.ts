import type { IfLetNode } from '../../builtin/specialExpressions/if_let'
import type { BindingNode } from '../../parser/interface'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateIfLetOutcomes: CalculatePossibleAstNodesHelper<IfLetNode> = ({
  astNode,
  nilNode,
  calculatePossibleAstNodes,
  combinateAstNodes,
  isAstComputable,
}) => {
  const bindingNode = astNode.b
  const thenBranch = astNode.p[0]!
  const elseBranch = astNode.p[1] ?? nilNode

  if (!isAstComputable(bindingNode.v)) {
    return [
      ...calculatePossibleAstNodes(thenBranch),
      ...calculatePossibleAstNodes(elseBranch),
    ]
  }

  const newIdentifier = bindingNode.n
  return calculatePossibleAstNodes(bindingNode.v)
    .map<BindingNode>(bindingValue => ({ ...bindingNode, v: bindingValue }))
    .flatMap<IfLetNode>(b => combinateAstNodes(astNode.p, [newIdentifier])
      .map<IfLetNode>(p => ({
        ...astNode,
        b,
        p,
      })),
    )
}
