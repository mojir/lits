import { LitsError } from '../../errors'
import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asNotUndefined, assertLength, isSeq, toAny } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface WhenFirstSpecialExpressionNode extends SpecialExpressionNode {
  name: `when-first`
  binding: BindingNode
}

export const whenFirstSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    const firstToken = asNotUndefined(tokens[position])
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    if (bindings.length !== 1) {
      throw new LitsError(`Expected exactly one binding, got ${bindings.length}`, firstToken.meta)
    }

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: WhenFirstSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `when-first`,
      binding: asNotUndefined(bindings[0]),
      params,
      token: firstToken,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castWhenFirstExpressionNode(node)
    const locals: Context = {}
    const evaluatedBindingForm = evaluateAstNode(node.binding.value, contextStack)
    if (!isSeq(evaluatedBindingForm)) {
      throw new LitsError(`Expected undefined or a sequence, got ${evaluatedBindingForm}`, node.token.meta)
    }

    if (evaluatedBindingForm.length === 0) {
      return null
    }

    const bindingValue = toAny(evaluatedBindingForm[0])
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

function castWhenFirstExpressionNode(_node: SpecialExpressionNode): asserts _node is WhenFirstSpecialExpressionNode {
  return
}
