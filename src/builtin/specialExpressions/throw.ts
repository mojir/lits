import { UnexpectedTokenError, UserDefinedError } from '../../errors'
import { AstNode, SpecialExpressionNode } from '../../parser/interface'
import { asNonEmptyString, asNotUndefined } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface ThrowSpecialExpressionNode extends SpecialExpressionNode {
  name: `throw`
  messageNode: AstNode
}

export const throwSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokens, position, { parseToken }) => {
    const firstToken = asNotUndefined(tokens[position])
    const [newPosition, messageNode] = parseToken(tokens, position)
    position = newPosition

    const token = asNotUndefined(tokens[position])
    if (!(token.type === `paren` && token.value === `)`)) {
      throw new UnexpectedTokenError(`)`, token)
    }
    position += 1

    const node: ThrowSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `throw`,
      params: [],
      messageNode,
      token: firstToken,
    }
    return [position, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castThrowExpressionNode(node)
    const message = asNonEmptyString(evaluateAstNode(node.messageNode, contextStack), node.token.meta)
    throw new UserDefinedError(message, node.token.meta)
  },
}

function castThrowExpressionNode(_node: SpecialExpressionNode): asserts _node is ThrowSpecialExpressionNode {
  return
}
