import { ReturnFromSignal } from '../../errors'
import { SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLengthOne } from '../../utils'
import { SpecialExpression } from '../interface'

interface ReturnFromSpecialExpressionNode extends SpecialExpressionNode {
  name: 'return-from'
  blockName: string,
}

export const returnFromSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    let token = asNotUndefined(tokens[position])
    if (token.type !== 'name') {
      throw Error(`Expected a name node, got ${token.type}: ${token.value}`)
    }

    const node: ReturnFromSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'return-from',
      blockName: token.value,
      params: [],
    }

    position += 1
    const [newPosition, valueNode] = parseToken(tokens, position)
    node.params.push(valueNode)
    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castReturnFromExpressionNode(node)
    const value = evaluateAstNode(asNotUndefined(node.params[0]), contextStack)
    throw new ReturnFromSignal(node.blockName, value)
  },
  validate: node => assertLengthOne(node.params),
}

function castReturnFromExpressionNode(_node: SpecialExpressionNode): asserts _node is ReturnFromSpecialExpressionNode {
  return
}
