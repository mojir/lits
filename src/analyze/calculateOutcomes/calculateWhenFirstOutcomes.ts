import type { WhenFirstNode } from '../../builtin/specialExpressions/when-first'
import type { BindingNode } from '../../parser/interface'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateWhenFirstOutcomes: CalculatePossibleAstNodesHelper<WhenFirstNode> = ({
  astNode,
  calculatePossibleAstNodes,
  combinateAstNodes,
  isAstComputable,
}) => {
  const bindingNode = astNode.b

  if (!isAstComputable(bindingNode.v))
    throw new Error('Could not calculate binding value')

  const newIdentifier = bindingNode.n
  return calculatePossibleAstNodes(bindingNode.v)
    .map<BindingNode>(bindingValue => ({ ...bindingNode, v: bindingValue }))
    .flatMap(b =>
      combinateAstNodes(astNode.p, [newIdentifier])
        .map<WhenFirstNode>(p => ({
          ...astNode,
          b,
          p,
        })),
    )
}
