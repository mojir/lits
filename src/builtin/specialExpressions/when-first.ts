import { Context } from '../../evaluator/interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLength, isSeq } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface WhenFirstSpecialExpressionNode extends SpecialExpressionNode {
  name: `when-first`
  binding: BindingNode
}

export const whenFirstSpecialExpression: BuiltinSpecialExpression = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    if (bindings.length !== 1) {
      throw Error(`Expected exactly one binding, got ${bindings.length}`)
    }

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: WhenFirstSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `when-first`,
      binding: asNotUndefined(bindings[0]),
      params,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castWhenFirstExpressionNode(node)
    const locals: Context = {}
    const evaluatedBindingForm = evaluateAstNode(node.binding.value, contextStack)
    if (!isSeq(evaluatedBindingForm)) {
      throw Error(`Expected undefined or a sequence, got ${evaluatedBindingForm}`)
    }

    if (evaluatedBindingForm.length === 0) {
      return undefined
    }

    const bindingValue = evaluatedBindingForm[0]
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

function castWhenFirstExpressionNode(_node: SpecialExpressionNode): asserts _node is WhenFirstSpecialExpressionNode {
  return
}
