import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLength } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface IfLetSpecialExpressionNode extends SpecialExpressionNode {
  name: `if-let`
  binding: BindingNode
}

export const ifLetSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    const firstToken = asNotUndefined(tokens[position])
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    if (bindings.length !== 1) {
      throw Error(`Expected exactly one binding, got ${bindings.length}`)
    }

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: IfLetSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `if-let`,
      binding: asNotUndefined(bindings[0]),
      params,
      token: firstToken,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castIfLetExpressionNode(node)
    const locals: Context = {}
    const bindingValue = evaluateAstNode(node.binding.value, contextStack)
    if (bindingValue) {
      locals[node.binding.name] = { value: bindingValue }
      const newContextStack = contextStack.withContext(locals)
      const thenForm = asNotUndefined(node.params[0])
      return evaluateAstNode(thenForm, newContextStack)
    }
    if (node.params.length === 2) {
      const elseForm = asNotUndefined(node.params[1])
      return evaluateAstNode(elseForm, contextStack)
    }
    return null
  },
  validate: node => assertLength({ min: 1, max: 2 }, node),
}

function castIfLetExpressionNode(_node: SpecialExpressionNode): asserts _node is IfLetSpecialExpressionNode {
  return
}
