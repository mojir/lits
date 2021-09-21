import { AstNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface WhileSpecialExpressionNode extends SpecialExpressionNode {
  name: 'while'
  whileExpression: AstNode
}

export const whileSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    let whileExpression: AstNode
    ;[position, whileExpression] = parseToken(tokens, position)

    const node: WhileSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'while',
      params: [],
      whileExpression,
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
    castWhileExpressionNode(node)
    while (evaluateAstNode(node.whileExpression, contextStack)) {
      for (const form of node.params) {
        evaluateAstNode(form, contextStack)
      }
    }
  },
}

function castWhileExpressionNode(_node: SpecialExpressionNode): asserts _node is WhileSpecialExpressionNode {
  return
}
