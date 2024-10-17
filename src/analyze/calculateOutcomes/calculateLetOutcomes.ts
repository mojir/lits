import type { DoNode } from '../../builtin/specialExpressions/do'
import type { LetNode } from '../../builtin/specialExpressions/let'
import { AstNodeType } from '../../constants/constants'
import type { BindingNode } from '../../parser/interface'
import { combinate } from '../utils'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateLetOutcomes: CalculatePossibleAstNodesHelper<LetNode> = ({
  astNode,
  calculatePossibleAstNodes,
  combinateAstNodes,
  isAstComputable,
}) => {
  try {
    // check bindings, if any binding value cannot be calculated, convert the whole let to a do-expression
    if (!isAstComputable(astNode.bs.map(b => calculatePossibleAstNodes(b.v))))
      throw new Error('Could not calculate binding value')
  }
  catch {
    const doNodes: DoNode[] = combinateAstNodes(astNode.p)
      .map((p) => {
        return {
          n: 'do',
          t: AstNodeType.SpecialExpression,
          p,
          debugData: astNode.debugData,
        }
      })
    return doNodes
  }

  const newIdentifiers = astNode.bs.map(bindingNode => bindingNode.n)
  const letNodes: LetNode[] = combinate(
    astNode.bs.map<BindingNode[]>(bindingNode =>
      calculatePossibleAstNodes(bindingNode.v)
        .map<BindingNode>(bindingValues => ({ ...bindingNode, v: bindingValues })),
    ),
  )
    .flatMap(bindingNodes => combinate(astNode.p.map(p => calculatePossibleAstNodes(p, newIdentifiers)))
      .map((p) => {
        return {
          n: 'let',
          bs: bindingNodes,
          t: AstNodeType.SpecialExpression,
          p,
          debugData: astNode.debugData,
        }
      }))

  return letNodes
}
