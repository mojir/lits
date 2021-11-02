import { LitsError, RecurSignal } from '../../errors'
import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asX } from '../../utils'
import { any, token } from '../../utils/assertion'
import { BuiltinSpecialExpression } from '../interface'

interface LoopSpecialExpressionNode extends SpecialExpressionNode {
  name: `loop`
  bindings: BindingNode[]
}

export const loopSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens, parseBindings }) => {
    const firstToken = token.as(tokens[position], `EOF`)
    let bindings: BindingNode[]
    ;[position, bindings] = parseBindings(tokens, position)

    let params: AstNode[]
    ;[position, params] = parseTokens(tokens, position)

    const node: LoopSpecialExpressionNode = {
      type: `SpecialExpression`,
      name: `loop`,
      params,
      bindings,
      token: firstToken,
    }
    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    const sourceCodeInfo = node.token.sourceCodeInfo
    castLoopExpressionNode(node)
    const bindingContext: Context = node.bindings.reduce((result: Context, binding) => {
      result[binding.name] = { value: evaluateAstNode(binding.value, contextStack) }
      return result
    }, {})
    const newContextStack = contextStack.withContext(bindingContext)

    for (;;) {
      let result: Any = null
      try {
        for (const form of node.params) {
          result = evaluateAstNode(form, newContextStack)
        }
      } catch (error) {
        if (error instanceof RecurSignal) {
          const params = error.params
          if (params.length !== node.bindings.length) {
            throw new LitsError(
              `recur expected ${node.bindings.length} parameters, got ${params.length}`,
              sourceCodeInfo,
            )
          }
          node.bindings.forEach((binding, index) => {
            asX(bindingContext[binding.name], sourceCodeInfo).value = any.as(params[index], sourceCodeInfo)
          })
          continue
        }
        throw error
      }
      return result
    }
  },
}

function castLoopExpressionNode(_node: SpecialExpressionNode): asserts _node is LoopSpecialExpressionNode {
  return
}
