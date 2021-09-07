import { normalExpressions } from '../normalExpressions'
import { functionSymbol, LispishFunction, SpecialExpressionNode } from '../../parser/interface'
import { asNameNode, asNotUndefined, assertLengthOne } from '../../utils'
import { SpecialExpression } from '../interface'

interface FunctionSpecialExpressionNode extends SpecialExpressionNode {
  name: 'function'
}

export const functionSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const [newPosition, parameter] = parseToken(tokens, position)
    if (parameter.type !== 'Name') {
      throw Error('Expected a name node')
    }

    position = newPosition

    const token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === ')')) {
      throw SyntaxError(`Invalid token "${token.type}" value=${token.value}, expected ')'`)
    }

    const node: FunctionSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'function',
      params: [parameter],
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack): LispishFunction => {
    castFunctionExpressionNode(node)

    const parameter = asNameNode(node.params[0])
    let lispishFunction: LispishFunction | undefined = undefined
    for (const context of contextStack) {
      lispishFunction = context.functions[parameter.value]
      if (lispishFunction) {
        break
      }
    }
    if (lispishFunction) {
      return lispishFunction
    }

    if (!normalExpressions[parameter.value]) {
      throw Error(`Could not find built in function (normal expresssion) ${parameter.value}`)
    }

    return {
      [functionSymbol]: true,
      builtin: parameter.value,
    }
  },
  validate: node => {
    assertLengthOne(node.params)
  },
}

function castFunctionExpressionNode(_node: SpecialExpressionNode): asserts _node is FunctionSpecialExpressionNode {
  return
}
