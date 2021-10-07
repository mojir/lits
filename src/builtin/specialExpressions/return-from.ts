import { ReturnFromSignal, UnexpectedTokenError } from '../../errors'
import { SpecialExpressionNode } from '../../parser/interface'
import { asAstNode, asNotUndefined, assertLength } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface ReturnFromSpecialExpressionNode extends SpecialExpressionNode {
  name: `return-from`
  blockName: string
}

export const returnFromSpecialExpression: BuiltinSpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const token = asNotUndefined(tokens[position])
    if (token.type !== `name`) {
      throw new UnexpectedTokenError(`name`, token)
    }

    const node: ReturnFromSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `return-from`,
      blockName: token.value,
      params: [],
    }

    position += 1
    const [newPosition, valueNode] = parseToken(tokens, position)
    node.params.push(valueNode)
    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castReturnFromExpressionNode(node)
    const value = evaluateAstNode(asAstNode(node.params[0]), contextStack)
    throw new ReturnFromSignal(node.blockName, value)
  },
  validate: node => assertLength(1, node),
}

function castReturnFromExpressionNode(_node: SpecialExpressionNode): asserts _node is ReturnFromSpecialExpressionNode {
  return
}
