import { joinAnalyzeResults } from '../../analyze/utils'
import { LitsError } from '../../errors'
import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams, asValue, token } from '../../utils/assertion'
import { valueToString } from '../../utils/helpers'
import { BuiltinSpecialExpression } from '../interface'

type WhenLetNode = SpecialExpressionNode & {
  binding: BindingNode
}

export const whenLetSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    if (bindings.length !== 1) {
      throw new LitsError(`Expected exactly one binding, got ${valueToString(bindings.length)}`, firstToken.debugInfo)
    }

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: WhenLetNode = {
      type: `SpecialExpression`,
      name: `when-let`,
      binding: asValue(bindings[0], firstToken.debugInfo),
      params,
      token: firstToken.debugInfo ? firstToken : undefined,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const { binding } = node as WhenLetNode
    const locals: Context = {}
    const bindingValue = evaluateAstNode(binding.value, contextStack)
    if (!bindingValue) {
      return null
    }
    locals[binding.name] = { value: bindingValue }
    const newContextStack = contextStack.withContext(locals)

    let result: Any = null
    for (const form of node.params) {
      result = evaluateAstNode(form, newContextStack)
    }
    return result
  },
  validate: node => assertNumberOfParams({ min: 0 }, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const { binding } = node as WhenLetNode
    const newContext: Context = { [binding.name]: { value: true } }
    const bindingResult = analyzeAst(binding.value, contextStack, builtin)
    const paramsResult = analyzeAst(node.params, contextStack.withContext(newContext), builtin)
    return joinAnalyzeResults(bindingResult, paramsResult)
  },
}
