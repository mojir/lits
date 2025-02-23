import type { TryNode } from '../../builtin/specialExpressions/try'
import type { ThrowNode } from '../../builtin/specialExpressions/throw'
import type { AstNode } from '../../parser/interface'
import type { LetNode } from '../../builtin/specialExpressions/let'
import { AstNodeType } from '../../constants/constants'
import type { CalculatePossibleAstNodesHelper } from '.'

export const calculateTryOutcomes: CalculatePossibleAstNodesHelper<TryNode> = ({
  astNode,
  calculatePossibleAstNodes,
}) => {
  const { vals, throws } = calculatePossibleAstNodes(astNode.p[0]!).reduce((acc: { vals: AstNode[], throws: AstNode[] }, node) => {
    if (node.n === 'throw') {
      acc.throws.push((node as ThrowNode).p[0]!)
    }
    else {
      acc.vals.push(node)
    }
    return acc
  }, { vals: [], throws: [] })

  const catches = throws.flatMap<AstNode>((throwNode) => {
    const letNode: LetNode = {
      t: AstNodeType.SpecialExpression,
      n: 'let',
      bs: [{
        t: AstNodeType.Binding,
        n: astNode.e.v,
        v: throwNode,
        token: undefined,
        p: [],
      }],
      p: [astNode.ce],
      token: undefined,
    }
    return calculatePossibleAstNodes(letNode)
  })

  return [
    ...vals,
    ...catches,
  ]
}
