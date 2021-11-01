import { LitsError } from '../../errors'
import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLength } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface WhenLetSpecialExpressionNode extends SpecialExpressionNode {
  name: `when-let`
  binding: BindingNode
}

export const whenLetSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    const firstToken = asNotUndefined(tokens[position], `EOF`)
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    if (bindings.length !== 1) {
      throw new LitsError(`Expected exactly one binding, got ${bindings.length}`, firstToken.meta)
    }

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: WhenLetSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `when-let`,
      binding: asNotUndefined(bindings[0], firstToken.meta),
      params,
      token: firstToken,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castWhenLetExpressionNode(node)
    const locals: Context = {}
    const bindingValue = evaluateAstNode(node.binding.value, contextStack)
    if (!bindingValue) {
      return null
    }
    locals[node.binding.name] = { value: bindingValue }
    const newContextStack = contextStack.withContext(locals)

    let result: Any = null
    for (const form of node.params) {
      result = evaluateAstNode(form, newContextStack)
    }
    return result
  },
  validate: node => assertLength({ min: 0 }, node),
}

function castWhenLetExpressionNode(_node: SpecialExpressionNode): asserts _node is WhenLetSpecialExpressionNode {
  return
}
