import { ReturnFromSignal, UnexpectedTokenError } from '../../errors'
import { Context } from '../../evaluator/interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLength } from '../../utils'
import { SpecialExpression } from '../interface'

interface BlockSpecialExpressionNode extends SpecialExpressionNode {
  name: 'block'
  blockName: string
}

export const blockSpecialExpression: SpecialExpression = {
  parse: (tokens, position, { parseToken }) => {
    let token = asNotUndefined(tokens[position])
    if (token.type !== 'name') {
      throw new UnexpectedTokenError('name', token)
    }

    position += 1

    const node: BlockSpecialExpressionNode = {
      type: 'SpecialExpression',
      name: 'block',
      blockName: token.value,
      params: [],
    }

    token = asNotUndefined(tokens[position])
    while (!(token.type === 'paren' && token.value === ')')) {
      const [newPosition, bodyNode] = parseToken(tokens, position)
      node.params.push(bodyNode)
      position = newPosition
      token = asNotUndefined(tokens[position])
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, evaluateAstNode) => {
    castBlockExpressionNode(node)
    const newContext: Context = {
      functions: {},
      variables: {},
    }

    const newContextStack = [newContext, ...contextStack]
    let result: unknown
    try {
      for (const form of node.params) {
        result = evaluateAstNode(form, newContextStack)
      }
      return result
    } catch (error) {
      if (error instanceof ReturnFromSignal && node.blockName === error.blockName) {
        return error.value
      }
      throw error
    }
  },
  validate: node => assertLength({ min: 1 }, node),
}

function castBlockExpressionNode(_node: SpecialExpressionNode): asserts _node is BlockSpecialExpressionNode {
  return
}
