import type { DefsNode } from '../../builtin/specialExpressions/defs'
import type { AstNode } from '../../parser/interface'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateDefsOutcomes: CalculatePossibleAstNodesHelper<DefsNode> = ({
  astNode,
  combinateAstNodes,
}) => {
  return combinateAstNodes(astNode.p)
    .map<DefsNode>(p => ({
      ...astNode,
      p: p as [AstNode, AstNode],
    }))
}
