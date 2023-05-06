import { joinUndefinedSymbols } from '../../analyze/undefinedSymbols/utils'
import { LitsError } from '../../errors'
import { Context } from '../../ContextStack/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { toAny } from '../../utils'
import { asValue, sequence, token } from '../../utils/assertion'
import { valueToString } from '../../utils/helpers'
import { BuiltinSpecialExpression } from '../interface'

type WhenFirstNode = SpecialExpressionNode & {
  binding: BindingNode
}

export const whenFirstSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    if (bindings.length !== 1) {
      throw new LitsError(`Expected exactly one binding, got ${valueToString(bindings.length)}`, firstToken.debugInfo)
    }

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: WhenFirstNode = {
      type: `SpecialExpression`,
      name: `when-first`,
      binding: asValue(bindings[0], firstToken.debugInfo),
      params,
      token: firstToken.debugInfo ? firstToken : undefined,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const locals: Context = {}
    const { binding } = node as WhenFirstNode
    const evaluatedBindingForm = evaluateAstNode(binding.value, contextStack)
    if (!sequence.is(evaluatedBindingForm)) {
      throw new LitsError(
        `Expected undefined or a sequence, got ${valueToString(evaluatedBindingForm)}`,
        node.token?.debugInfo,
      )
    }

    if (evaluatedBindingForm.length === 0) {
      return null
    }

    const bindingValue = toAny(evaluatedBindingForm[0])
    locals[binding.name] = { value: bindingValue }
    const newContextStack = contextStack.withContext(locals)

    let result: Any = null
    for (const form of node.params) {
      result = evaluateAstNode(form, newContextStack)
    }
    return result
  },
  validateArity: () => undefined,
  findUndefinedSymbols: (node, contextStack, { findUndefinedSymbols, builtin }) => {
    const { binding } = node as WhenFirstNode
    const newContext: Context = { [binding.name]: { value: true } }
    const bindingResult = findUndefinedSymbols(binding.value, contextStack, builtin)
    const paramsResult = findUndefinedSymbols(node.params, contextStack.withContext(newContext), builtin)
    return joinUndefinedSymbols(bindingResult, paramsResult)
  },
}
