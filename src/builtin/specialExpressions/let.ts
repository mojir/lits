import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { BuiltinSpecialExpression } from '../interface'

interface LetSpecialExpressionNode extends SpecialExpressionNode {
  name: `let`
  bindings: BindingNode[]
}

export const letSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: LetSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `let`,
      params,
      bindings,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castLetExpressionNode(node)
    const locals: Context = {}
    const newContextStack = [locals, ...contextStack]
    for (const binding of node.bindings) {
      const bindingValueNode = binding.value
      const bindingValue = evaluateAstNode(bindingValueNode, newContextStack)
      locals[binding.name] = { value: bindingValue }
    }

    let result: Any = null
    for (const astNode of node.params) {
      result = evaluateAstNode(astNode, newContextStack)
    }
    return result
  },
}

function castLetExpressionNode(_node: SpecialExpressionNode): asserts _node is LetSpecialExpressionNode {
  return
}
