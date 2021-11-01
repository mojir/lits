import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNotUndefined, assertLength, assertString } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

interface DefsSpecialExpressionNode extends SpecialExpressionNode {
  name: `defs`
}

export const defsSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = asNotUndefined(tokens[position], `EOF`)
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
    const meta = node.token.meta
    const name = evaluateAstNode(asAstNode(node.params[0], meta), contextStack)
    assertString(name, meta)

    assertNameNotDefined(name, contextStack, builtin, node.token.meta)

    const value = evaluateAstNode(asAstNode(node.params[1], meta), contextStack)

    contextStack.globalContext[name] = { value }

    return value
  },
  validate: node => assertLength(2, node),
}

function castDefsExpressionNode(_node: SpecialExpressionNode): asserts _node is DefsSpecialExpressionNode {
  return
}
