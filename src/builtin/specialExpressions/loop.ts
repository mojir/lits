import { ReturnSignal } from '../../errors'
import { SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLengthOneOrMore } from '../../utils'
import { SpecialExpression } from '../interface'

interface LoopSpecialExpressionNode extends SpecialExpressionNode {
  name: 'loop'
}

export const loopSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const node: LoopSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'loop',
      params: [],
    }

    let token = asNotUndefined(tokens[position])
    while (!(token.type === 'paren' && token.value === ')')) {
      const [newPosition, bodyNode] = parseToken(tokens, position)
      node.params.push(bodyNode)
      position = newPosition
      token = asNotUndefined(tokens[position])
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castLoopExpressionNode(node)
    try {
      for (;;) {
        for (const form of node.params) {
          evaluateAstNode(form, contextStack)
        }
      }
    } catch (error) {
      if (error instanceof ReturnSignal) {
        return error.value
      }
      throw error
    }
  },
  validate: node => assertLengthOneOrMore(node.params),
}

function castLoopExpressionNode(_node: SpecialExpressionNode): asserts _node is LoopSpecialExpressionNode {
  return
}
