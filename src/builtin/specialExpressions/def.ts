import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNameNode, asNotUndefined, assertLength, assertNameNode } from '../../utils'
import { SpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

interface DefSpecialExpressionNode extends SpecialExpressionNode {
  name: `def`
}

export const defSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseTokens }) => {
    const [newPosition, params] = parseTokens(tokens, position)
    assertNameNode(params[0])
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `def`,
        params,
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castDefExpressionNode(node)
    const name = asNameNode(node.params[0]).value

    assertNameNotDefined(name, contextStack)

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)
    const context = asNotUndefined(contextStack[contextStack.length - 2])

    context[name] = { value }

    return value
  },
  validate: node => assertLength(2, node),
}

function castDefExpressionNode(_node: SpecialExpressionNode): asserts _node is DefSpecialExpressionNode {
  return
}
