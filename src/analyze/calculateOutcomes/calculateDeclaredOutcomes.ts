import type { DeclaredNode } from '../../builtin/specialExpressions/declared'
import type { AstNode } from '../../parser/interface'
import { AstNodeType } from '../../constants/constants'
import type { CalculatePossibleAstNodesHelper } from '.'

const trueNode: AstNode = { t: AstNodeType.ReservedName, v: 'true', token: undefined, p: [], n: undefined }
const falseNode: AstNode = { t: AstNodeType.ReservedName, v: 'false', token: undefined, p: [], n: undefined }

export const calculateDeclaredOutcomes: CalculatePossibleAstNodesHelper<DeclaredNode> = (
  {
    astNode,
    isAstComputable,
  },
) => {
  if (isAstComputable(astNode.p))
    return [trueNode]

  return [trueNode, falseNode]
}
