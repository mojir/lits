import type { AndNode } from '../../builtin/specialExpressions/and'
import { AstNodeType } from '../../constants/constants'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateAndOutcomes: CalculatePossibleAstNodesHelper<AndNode> = ({
  astNode,
  combinateAstNodes,
}) => {
  return combinateAstNodes(astNode.p)
    .map<AndNode>(p => ({
      n: '&&',
      t: AstNodeType.SpecialExpression,
      p,
      token: astNode.token,
    }))
}
