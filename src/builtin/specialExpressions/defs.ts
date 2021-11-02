import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { assertLength } from '../../utils'
import { astNode, string, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

interface DefsSpecialExpressionNode extends SpecialExpressionNode {
  name: `defs`
}

export const defsSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `defs`,
        params,
        token: firstToken,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    castDefsExpressionNode(node)
    const sourceCodeInfo = node.token.sourceCodeInfo
    const name = evaluateAstNode(astNode.as(node.params[0], sourceCodeInfo), contextStack)
    string.assert(name, sourceCodeInfo)

    assertNameNotDefined(name, contextStack, builtin, node.token.sourceCodeInfo)

    const value = evaluateAstNode(astNode.as(node.params[1], sourceCodeInfo), contextStack)

    contextStack.globalContext[name] = { value }

    return value
  },
  validate: node => assertLength(2, node),
}

function castDefsExpressionNode(_node: SpecialExpressionNode): asserts _node is DefsSpecialExpressionNode {
  return
}
