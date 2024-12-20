import type { OrNode } from '../../builtin/specialExpressions/or'
import { AstNodeType } from '../../constants/constants'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateOrOutcomes: CalculatePossibleAstNodesHelper<OrNode> = ({
  astNode,
  combinateAstNodes,
}) => {
  return combinateAstNodes(astNode.p)
    .map<OrNode>(p => ({
      n: 'or',
      t: AstNodeType.SpecialExpression,
      p,
      debugData: astNode.debugData,
    }))
}