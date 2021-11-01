import { LitsError, RecurSignal } from '../../errors'
import { Context } from '../../evaluator/interface'
import { Any } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { asAny, asNotUndefined } from '../../utils'
import { BuiltinSpecialExpression } from '../interface'

interface LoopSpecialExpressionNode extends SpecialExpressionNode {
  name: `loop`
  bindings: BindingNode[]
}

export const loopSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, { parseTokens, parseBindings }) => {
    const firstToken = asNotUndefined(tokens[position], `EOF`)
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
    const meta = node.token.meta
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
            throw new LitsError(`recur expected ${node.bindings.length} parameters, got ${params.length}`, meta)
          }
          node.bindings.forEach((binding, index) => {
            asNotUndefined(bindingContext[binding.name], meta).value = asAny(params[index], meta)
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
