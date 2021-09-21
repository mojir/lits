import { UserDefinedError } from '../../errors'
import { AstNode, SpecialExpressionNode } from '../../parser/interface'
import { asNonEmptyString, asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface ThrowSpecialExpressionNode extends SpecialExpressionNode {
  name: 'throw'
  messageNode: AstNode
}

export const throwSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const [newPosition, messageNode] = parseToken(tokens, position)
    position = newPosition

    const token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === ')')) {
      throw Error('Expected ")"')
    }
    position += 1

    const node: ThrowSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'throw',
      params: [],
      messageNode,
    }
    return [position, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castThrowExpressionNode(node)
    const message = asNonEmptyString(evaluateAstNode(node.messageNode, contextStack))
    throw new UserDefinedError(message)
  },
}

function castThrowExpressionNode(_node: SpecialExpressionNode): asserts _node is ThrowSpecialExpressionNode {
  return
}
