import { joinAnalyzeResults } from '../../analyze/utils'
import { LitsError } from '../../errors'
import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { assertNumberOfParams, astNode, asValue, token } from '../../utils/assertion'
import { valueToString } from '../../utils/helpers'
import { BuiltinSpecialExpression } from '../interface'

type IfLetNode = SpecialExpressionNode & {
  binding: BindingNode
}

export const ifLetSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    if (bindings.length !== 1) {
      throw new LitsError(`Expected exactly one binding, got ${valueToString(bindings.length)}`, firstToken.debugInfo)
    }

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: IfLetNode = {
      type: `SpecialExpression`,
      name: `if-let`,
      binding: asValue(bindings[0], firstToken.debugInfo),
      params,
      token: firstToken.debugInfo ? firstToken : undefined,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const debugInfo = node.token?.debugInfo
    const locals: Context = {}
    const bindingValue = evaluateAstNode((node as IfLetNode).binding.value, contextStack)
    if (bindingValue) {
      locals[(node as IfLetNode).binding.name] = { value: bindingValue }
      const newContextStack = contextStack.withContext(locals)
      const thenForm = astNode.as(node.params[0], debugInfo)
      return evaluateAstNode(thenForm, newContextStack)
    }
    if (node.params.length === 2) {
      const elseForm = astNode.as(node.params[1], debugInfo)
      return evaluateAstNode(elseForm, contextStack)
    }
    return null
  },
  validate: node => assertNumberOfParams({ min: 1, max: 2 }, node),
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const newContext: Context = { [(node as IfLetNode).binding.name]: { value: true } }
    const bindingResult = analyzeAst((node as IfLetNode).binding.value, contextStack, builtin)
    const paramsResult = analyzeAst(node.params, contextStack.withContext(newContext), builtin)
    return joinAnalyzeResults(bindingResult, paramsResult)
  },
}
