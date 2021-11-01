import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLength, assertNotUndefined } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface TimeSpecialExpressionNode extends SpecialExpressionNode {
  name: `time!`
}

export const timeSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseToken }) => {
    const firstToken = asNotUndefined(tokens[position], `EOF`)
    const [newPosition, astNode] = parseToken(tokens, position)
    const node: TimeSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `time!`,
      params: [astNode],
      token: firstToken,
    }

    return [newPosition + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castTimeExpressionNode(node)

    const [astNode] = node.params
    assertNotUndefined(astNode, node.token.meta)

    const startTime = Date.now()
    const result = evaluateAstNode(astNode, contextStack)
    const totalTime = Date.now() - startTime
    // eslint-disable-next-line no-console
    console.log(`Elapsed time: ${totalTime} ms`)

    return result
  },
  validate: node => assertLength(1, node),
}

function castTimeExpressionNode(_node: SpecialExpressionNode): asserts _node is TimeSpecialExpressionNode {
  return
}
