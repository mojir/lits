import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface doSpecialExpressionNode extends SpecialExpressionNode {
  name: `do`
}

export const doSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseToken }) => {
    let tkn = asNotUndefined(tokens[position], `EOF`)

    const node: doSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `do`,
      params: [],
      token: tkn,
    }

    while (!(tkn.type === `paren` && tkn.value === `)`)) {
      let bodyNode: AstNode
      ;[position, bodyNode] = parseToken(tokens, position)
      node.params.push(bodyNode)
      tkn = asNotUndefined(tokens[position], `EOF`)
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castDoExpressionNode(node)
    const newContext: Context = {}

    const newContextStack = contextStack.withContext(newContext)
    let result: Any = null
    for (const form of node.params) {
      result = evaluateAstNode(form, newContextStack)
    }
    return result
  },
}

function castDoExpressionNode(_node: SpecialExpressionNode): asserts _node is doSpecialExpressionNode {
  return
}
