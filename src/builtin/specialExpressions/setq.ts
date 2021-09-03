import { Context } from '../../evaluator/interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { ReservedName, reservedNamesRecord } from '../../reservedNames'
import { asAstNode, asNameNode, assertLengthTwo } from '../../utils'
import { SpecialExpression } from '../interface'

interface SetqSpecialExpressionNode extends SpecialExpressionNode {
  name: 'setq'
}

export const setqSpecialExpression: SpecialExpression = {
  parse: (_tokens, position) => {
    return [
      position,
      {
        type: 'SpecialExpression',
        name: 'setq',
        params: [],
      },
    ]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    assertSetqExpressionNode(node)
    const name = asNameNode(node.params[0]).value
    if (reservedNamesRecord[name as ReservedName]) {
      throw SyntaxError(`Cannot set symbol name to "${name}", it's a reserved name`)
    }

    const value = evaluateAstNode(asAstNode(node.params[1]), contextStack)

    // The second last stack entry is the "global" scope
    let context: Context | undefined = undefined
    for (let i = 0; i < contextStack.length - 1; i += 1) {
      if (Object.getOwnPropertyDescriptor(contextStack[i], name)) {
        context = contextStack[i]
        break
      }
    }

    // The second last stack entry is the "global" scope
    context = context || (contextStack[contextStack.length - 2] as Context)

    context[name] = value

    return value
  },
  validate: node => {
    assertSetqExpressionNode(node)
    assertLengthTwo(node.params)
  },
}

function assertSetqExpressionNode(node: SpecialExpressionNode): asserts node is SetqSpecialExpressionNode {
  if (node.name !== 'setq') {
    throw Error('Expected setq special expression node')
  }
}
