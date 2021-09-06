import { functionSymbol, LispishFunction, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLengthOne } from '../../utils'
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
    assertFunctionExpressionNode(node)

    const parameter = asNotUndefined(node.params[0])
    if (parameter.type === 'Name') {
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

      return {
        [functionSymbol]: true,
        builtin: parameter.value,
      }
    }
    throw Error('Unexpected function parameter, expected a name')
  },
  validate: node => {
    assertFunctionExpressionNode(node)
    assertLengthOne(node.params)
  },
}

function assertFunctionExpressionNode(node: SpecialExpressionNode): asserts node is FunctionSpecialExpressionNode {
  if (node.name !== 'function') {
    throw Error('Expected function special expression node')
  }
}
