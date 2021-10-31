import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNameNode, asNotUndefined, assertLength, assertNameNode } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

interface DefSpecialExpressionNode extends SpecialExpressionNode {
  name: `def`
}

export const defSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = asNotUndefined(tokens[position])
    const [newPosition, params] = parseTokens(tokens, position)
    assertNameNode(params[0])
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `def`,
        params,
        token: firstToken,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    castDefExpressionNode(node)
    const name = asNameNode(node.params[0]).value

    assertNameNotDefined(name, contextStack, builtin)

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    contextStack.globalContext[name] = { value }

    return value
  },
  validate: node => assertLength(2, node),
}

function castDefExpressionNode(_node: SpecialExpressionNode): asserts _node is DefSpecialExpressionNode {
  return
}
