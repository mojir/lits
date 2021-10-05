import { builtin } from '..'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNameNode, asNotUndefined, assertLength, assertNameNode } from '../../utils'
import { SpecialExpression } from '../interface'

interface DefSpecialExpressionNode extends SpecialExpressionNode {
  name: `def`
}

export const defSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
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

function castDefExpressionNode(_node: SpecialExpressionNode): asserts _node is DefSpecialExpressionNode {
  return
}
