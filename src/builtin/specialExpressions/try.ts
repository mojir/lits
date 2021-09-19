import { ReturnFromSignal, ReturnSignal } from '../../errors'
import { Context } from '../../evaluator/interface'
import { AstNode, NameNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { SpecialExpression } from '../interface'

interface TrySpecialExpressionNode extends SpecialExpressionNode {
  name: 'try'
  tryExpression: AstNode
  error: NameNode
  catchExpression: AstNode
}

export const trySpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    const [newPosition1, tryExpression] = parseToken(tokens, position)
    position = newPosition1

    let token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw Error('Expected "("')
    }
    position += 1
    token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === '(')) {
      throw Error('Expected "("')
    }
    position += 1
    const [newPosition2, error] = parseToken(tokens, position)
    if (error.type !== 'Name') {
      throw Error(`Expected name node, got: ${error.type}`)
    }
    position = newPosition2
    token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === ')')) {
      throw Error('Expected ")"')
    }
    position += 1

    const [newPosition3, catchExpression] = parseToken(tokens, position)
    position = newPosition3

    token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === ')')) {
      throw Error('Expected ")"')
    }
    position += 1

    token = asNotUndefined(tokens[position])
    if (!(token.type === 'paren' && token.value === ')')) {
      throw Error('Expected ")"')
    }
    position += 1

    const node: TrySpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'try',
      params: [],
      tryExpression,
      catchExpression,
      error
    }

    return [position, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castTryExpressionNode(node)
    try {
      return evaluateAstNode(node.tryExpression, contextStack)
    } catch (error) {
      if (error instanceof ReturnFromSignal || error instanceof ReturnSignal) {
        throw error
      }
      const newContext: Context = { functions: {}, variables: { [node.error.value]: error } }
      return evaluateAstNode(node.catchExpression, [newContext, ...contextStack])
    }
  },
}

function castTryExpressionNode(_node: SpecialExpressionNode): asserts _node is TrySpecialExpressionNode {
  return
}
