import { Context } from '../../evaluator/interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { isLispishFunction } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface LetSpecialExpressionNode extends SpecialExpressionNode {
  name: `let`
  bindings: BindingNode[]
}

export const letSpecialExpression: BuiltinSpecialExpression = {
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
    for (const binding of node.bindings) {
      const bindingValueNode = binding.value
      const bindingValue = evaluateAstNode(bindingValueNode, contextStack)
      if (isLispishFunction(bindingValue)) {
        throw Error(`Cannot bind function in let expression`)
      }
      locals[binding.name] = { value: bindingValue }
    }
    const newContextStack = [locals, ...contextStack]

    let result: unknown
    for (const astNode of node.params) {
      result = evaluateAstNode(astNode, newContextStack)
    }
    return result
  },
}

function castLetExpressionNode(_node: SpecialExpressionNode): asserts _node is LetSpecialExpressionNode {
  return
}
