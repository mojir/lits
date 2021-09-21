import { ReturnFromSignal } from '../../errors'
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
      throw Error(`Expected a name node, got ${token.type}: ${token.value}`)
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
    let result: unknown
    try {
      for (const form of node.params) {
        result = evaluateAstNode(form, contextStack)
      }
      return result
    } catch (error) {
      if (error instanceof ReturnFromSignal && node.blockName === error.blockName) {
        return error.value
      }
      throw error
    }
  },
  validate: node => assertLength({ min: 1 }, node.params),
}

function castBlockExpressionNode(_node: SpecialExpressionNode): asserts _node is BlockSpecialExpressionNode {
  return
}
