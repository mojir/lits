import type { DoNode } from '../../builtin/specialExpressions/do'
import type { WhenLetNode } from '../../builtin/specialExpressions/when_let'
import type { BindingNode } from '../../parser/interface'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateWhenLetOutcomes: CalculatePossibleAstNodesHelper<WhenLetNode> = ({
  astNode,
  nilNode,
  calculatePossibleAstNodes,
  combinateAstNodes,
  isAstComputable,
}) => {
  const bindingNode = astNode.b

  if (!isAstComputable(bindingNode.v)) {
    return [
      ...combinateAstNodes(astNode.p)
        .map<DoNode>(p => ({
          n: 'do',
          t: astNode.t,
          p,
          token: astNode.token,
        })),
      nilNode,
    ]
  }

  const newIdentifier = bindingNode.n
  return calculatePossibleAstNodes(bindingNode.v)
    .map<BindingNode>(bindingValue => ({ ...bindingNode, v: bindingValue }))
    .flatMap(b =>
      combinateAstNodes(astNode.p, [newIdentifier])
        .map<WhenLetNode>(p => ({
          ...astNode,
          b,
          p,
        })),
    )
}
