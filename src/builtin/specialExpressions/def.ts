import { Any } from '../../interface'
import { SpecialExpressionNode } from '../../parser/interface'
import { asNameNode, asNotUndefined, assertLength, assertNameNode } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'
import { assertNameNotDefined } from '../utils'

interface DefSpecialExpressionNode extends SpecialExpressionNode {
  name: `def`
}

export const defSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens }) => {
    const firstToken = asNotUndefined(tokens[position], `EOF`)
    const [newPosition, params] = parseTokens(tokens, position)
    assertNameNode(params[0], firstToken.sourceCodeInfo)
    return [
      newPosition + 1,
      {
        type: `SpecialExpression`,
        name: `def`,
        params,
        token: firstToken,
      },
    ]
  },
  evaluate: (node, contextStack, { evaluateAstNode, builtin }) => {
    castDefExpressionNode(node)
    const sourceCodeInfo = node.token.sourceCodeInfo
    const name = asNameNode(node.params[0], sourceCodeInfo).value

    assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo)

    const value = evaluateAstNode(asNotUndefined(node.params[1], sourceCodeInfo), contextStack)

    contextStack.globalContext[name] = { value }

    return value
  },
  validate: node => assertLength(2, node),
}

function castDefExpressionNode(_node: SpecialExpressionNode): asserts _node is DefSpecialExpressionNode {
  return
}
