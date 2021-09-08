import { Context } from '../../evaluator/interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNameNode, asNotUndefined, assertLengthTwo, assertNameNode } from '../../utils'
import { SpecialExpression } from '../interface'

interface SetqSpecialExpressionNode extends SpecialExpressionNode {
  name: 'setq'
}

export const setqSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseParams }) => {
    const [newPosition, params] = parseParams(tokens, position)
    assertNameNode(params[0])
    return [
      newPosition + 1,
      {
        type: 'SpecialExpression',
        name: 'setq',
        params,
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castSetqExpressionNode(node)
    const name = asNameNode(node.params[0]).value

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    // The second last stack entry is the "global" scope
    let context: Context | undefined = undefined
    for (let i = 0; i < contextStack.length - 1; i += 1) {
      if (Object.getOwnPropertyDescriptor(asNotUndefined(contextStack[i]).variables, name)) {
        context = contextStack[i]
        break
      }
    }

    if (!context) {
      // The second last stack entry is the "global" scope
      context = asNotUndefined(contextStack[contextStack.length - 2])
    }
    context.variables[name] = value

    return value
  },
  validate: node => {
    assertLengthTwo(node.params)
  },
}

function castSetqExpressionNode(_node: SpecialExpressionNode): asserts _node is SetqSpecialExpressionNode {
  return
}
