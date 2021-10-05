import { builtin } from '..'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNotUndefined, assertLength, assertString } from '../../utils'
import { SpecialExpression } from '../interface'

interface DefsSpecialExpressionNode extends SpecialExpressionNode {
  name: `defs`
}

export const defsSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
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

    if (builtin.specialExpressions[name]) {
      throw Error(`Cannot define variable ${name}, it's a special expression`)
    }

    if (builtin.normalExpressions[name]) {
      throw Error(`Cannot define variable ${name}, it's a builtin function`)
    }

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    const context = asNotUndefined(contextStack[contextStack.length - 2])

    if (context[name]) {
      throw Error(`Variable already exists "${name}"`)
    }

    context[name] = { value }

    return value
  },
  validate: node => assertLength(2, node),
}

function castDefsExpressionNode(_node: SpecialExpressionNode): asserts _node is DefsSpecialExpressionNode {
  return
}
