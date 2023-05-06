import { joinAnalyzeResults } from '../../analyze/utils'
import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

type LetNode = SpecialExpressionNode & {
  bindings: BindingNode[]
}

export const letSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseBindings, parseTokens }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: LetNode = {
      type: `SpecialExpression`,
      name: `let`,
      params,
      bindings,
      token: firstToken.debugInfo ? firstToken : undefined,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const locals: Context = {}
    const newContextStack = contextStack.withContext(locals)
    for (const binding of (node as LetNode).bindings) {
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
  analyze: (node, contextStack, { analyzeAst, builtin }) => {
    const newContext = (node as LetNode).bindings
      .map(binding => binding.name)
      .reduce((context: Context, name) => {
        context[name] = { value: true }
        return context
      }, {})
    const bindingContext: Context = {}
    const bindingResults = (node as LetNode).bindings.map(bindingNode => {
      const valueNode = bindingNode.value
      const bindingsResult = analyzeAst(valueNode, contextStack.withContext(bindingContext), builtin)
      bindingContext[bindingNode.name] = { value: true }
      return bindingsResult
    })

    const paramsResult = analyzeAst(node.params, contextStack.withContext(newContext), builtin)
    return joinAnalyzeResults(...bindingResults, paramsResult)
  },
}
