import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNotUndefined, assertLength, assertString } from '../../utils'
import { SpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

interface DefsSpecialExpressionNode extends SpecialExpressionNode {
  name: `defs`
}

export const defsSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseTokens }) => {
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `defs`,
        params,
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castDefsExpressionNode(node)
    const name = evaluateAstNode(asAstNode(node.params[0]), contextStack)
    assertString(name)

    assertNameNotDefined(name, contextStack)

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)
    const context = asNotUndefined(contextStack[contextStack.length - 2])

    context[name] = { value }

    return value
  },
  validate: node => assertLength(2, node),
}

function castDefsExpressionNode(_node: SpecialExpressionNode): asserts _node is DefsSpecialExpressionNode {
  return
}
