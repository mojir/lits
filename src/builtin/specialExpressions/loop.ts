import { LitsError, RecurSignal } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { BindingNode, Node, SpecialExpressionNode } from '../../parser/types'
import { asAny } from '../../typeGuards/lits'
import { joinSets } from '../../utils'
import { valueToString } from '../../utils/debug/debugTools'
import { evalueateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
import type { BuiltinSpecialExpression } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type LoopNode = SpecialExpressionNode<[typeof specialExpressionTypes['loop'], BindingNode[], Node[]]> // bindings, body

export const loopSpecialExpression: BuiltinSpecialExpression<Any, LoopNode> = {
  paramCount: {},
  evaluate: (node, contextStack, { evaluateNode }) => {
    const bindingNodes = node[1][1]
    const bindingContext: Context = bindingNodes.reduce((result: Context, bindingNode) => {
      const val = evaluateNode(bindingNode[1][1], contextStack.create(result))
      const valueRecord = evalueateBindingNodeValues(bindingNode[1][0], val, Node => evaluateNode(Node, contextStack))
      Object.entries(valueRecord).forEach(([name, value]) => {
        result[name] = { value }
      })
      return result
    }, {})
    const newContextStack = contextStack.create(bindingContext)

    const body = node[1][2]
    for (;;) {
      let result: Any = null
      try {
        for (const form of body) {
          result = evaluateNode(form, newContextStack)
        }
      }
      catch (error) {
        if (error instanceof RecurSignal) {
          const params = error.params
          if (params.length !== bindingNodes.length) {
            throw new LitsError(
              `recur expected ${bindingNodes.length} parameters, got ${valueToString(params.length)}`,
              node[2],
            )
          }
          bindingNodes.forEach((bindingNode, index) => {
            const valueRecord = evalueateBindingNodeValues(bindingNode[1][0], asAny(params[index]), Node => evaluateNode(Node, contextStack))
            for (const [name, value] of Object.entries(valueRecord)) {
              bindingContext[name]!.value = value
            }
          })
          continue
        }
        throw error
      }
      return result
    }
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const bindingNodes = node[1][1]

    const newContext = bindingNodes
      .reduce((context: Context, bindingNode) => {
        const names = getAllBindingTargetNames(bindingNode[1][0])

        Object.keys(names).forEach((name) => {
          context[name] = { value: true }
        })
        return context
      }, {})

    const bindingValueNodes = bindingNodes.map(bindingNode => bindingNode[1][1])
    const bindingsResult = getUndefinedSymbols(bindingValueNodes, contextStack, builtin, evaluateNode)
    const paramsResult = getUndefinedSymbols(node[1][2], contextStack.create(newContext), builtin, evaluateNode)
    return joinSets(bindingsResult, paramsResult)
  },
}
