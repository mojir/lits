import { Context } from '../../evaluator/interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLength } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface WhenLetSpecialExpressionNode extends SpecialExpressionNode {
  name: `when-let`
  binding: BindingNode
}

export const whenLetSpecialExpression: BuiltinSpecialExpression = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    if (bindings.length !== 1) {
      throw Error(`Expected exactly one binding, got ${bindings.length}`)
    }

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: WhenLetSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `when-let`,
      binding: asNotUndefined(bindings[0]),
      params,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castWhenLetExpressionNode(node)
    const locals: Context = {}
    const bindingValue = evaluateAstNode(node.binding.value, contextStack)
    if (!bindingValue) {
      return undefined
    }
    locals[node.binding.name] = { value: bindingValue }
    const newContextStack = [locals, ...contextStack]

    let result: unknown = undefined
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
