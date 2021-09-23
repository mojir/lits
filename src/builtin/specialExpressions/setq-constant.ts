import { Context } from '../../evaluator/interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNameNode, asNotUndefined, assertLength, assertNameNode } from '../../utils'
import { SpecialExpression } from '../interface'

interface SetqConstantSpecialExpressionNode extends SpecialExpressionNode {
  name: 'setq-constant'
}

export const setqConstantSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
    assertNameNode(params[0])
    return [
      newPosition + 1,
      {
        type: 'SpecialExpression',
        name: 'setq-constant',
        params,
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castSetqConstantExpressionNode(node)
    const name = asNameNode(node.params[0]).value

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    // The second last stack entry is the "global" scope
    let context: Context | undefined = undefined
    for (let i = 0; i < contextStack.length - 1; i += 1) {
      if (contextStack[i]?.variables?.[name]) {
        context = contextStack[i]
        break
      }
    }

    if (!context) {
      // The second last stack entry is the "global" scope
      context = asNotUndefined(contextStack[contextStack.length - 2], 'This cannot be')
    }

    if (context.variables[name]?.const) {
      throw Error(`Cannot change constant variable "${name}"`)
    }
    context.variables[name] = { value, const: true }

    return value
  },
  validate: node => {
    assertLength(2, node)
  },
}

function castSetqConstantExpressionNode(
  _node: SpecialExpressionNode,
): asserts _node is SetqConstantSpecialExpressionNode {
  return
}
