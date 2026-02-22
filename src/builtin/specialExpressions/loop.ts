import { LitsError, RecurSignal } from '../../errors'
import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/types'
import { asAny } from '../../typeGuards/lits'
import { joinSets } from '../../utils'
import { valueToString } from '../../utils/debug/debugTools'
import type { MaybePromise } from '../../utils/maybePromise'
import { chain, reduceSequential, tryCatch } from '../../utils/maybePromise'
import { evaluateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type LoopNode = SpecialExpressionNode<[typeof specialExpressionTypes['loop'], BindingNode[], AstNode]> // bindings, body

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['loop (bindings) -> body'],
  details: [
    ['bindings', 'binding pairs', 'Comma-separated bindings with initial values, e.g. `n = 10, acc = 0`.'],
    ['body', 'expression', 'The expression to evaluate repeatedly. Use `recur` to loop back with new values.'],
  ],
  description: `Creates a loop with initial bindings. Use \`recur\` inside the body to jump back to the loop head with new binding values.

If \`recur\` is not called, the loop terminates and returns the value of the body expression.`,
  examples: [
    `loop (n = 10, acc = 0) -> do
  if n == 0 then
    acc
  else
    recur(n - 1, acc + n)
  end
end`,
    `loop (n = 5, acc = 1) -> do
  if n <= 1 then
    acc
  else
    recur(n - 1, acc * n)
  end
end`,
  ],
}

export const loopSpecialExpression: BuiltinSpecialExpression<Any, LoopNode> = {
  arity: {},
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const bindingNodes = node[1][1]

    // Set up initial binding context sequentially (bindings may depend on each other)
    const initialContext: Context = {}
    const setupBindings = reduceSequential(
      bindingNodes,
      (result: Context, bindingNode) => {
        return chain(evaluateNode(bindingNode[1][1], contextStack.create(result)), (val) => {
          const valueRecord = evaluateBindingNodeValues(bindingNode[1][0], val, Node => evaluateNode(Node, contextStack) as Any)
          Object.entries(valueRecord).forEach(([name, value]) => {
            result[name] = { value }
          })
          return result
        })
      },
      initialContext,
    )

    return chain(setupBindings, (bindingContext) => {
      const newContextStack = contextStack.create(bindingContext)
      const body = node[1][2]

      function rebindAndIterate(params: unknown[]): MaybePromise<Any> {
        if (params.length !== bindingNodes.length) {
          throw new LitsError(
            `recur expected ${bindingNodes.length} parameters, got ${valueToString(params.length)}`,
            node[2],
          )
        }
        bindingNodes.forEach((bindingNode, index) => {
          const valueRecord = evaluateBindingNodeValues(bindingNode[1][0], asAny(params[index]), Node => evaluateNode(Node, contextStack) as Any)
          for (const [name, value] of Object.entries(valueRecord)) {
            bindingContext[name]!.value = value
          }
        })
        return iterate()
      }

      function iterate(): MaybePromise<Any> {
        return tryCatch(
          () => evaluateNode(body, newContextStack),
          (error) => {
            if (error instanceof RecurSignal) {
              return rebindAndIterate(error.params)
            }
            throw error
          },
        )
      }

      // Use sync for(;;) loop for the sync case to avoid stack overflow
      for (;;) {
        try {
          const result = evaluateNode(body, newContextStack)
          if (result instanceof Promise) {
            // Async path: handle recur via promise chain
            return result.catch((error: unknown) => {
              if (error instanceof RecurSignal) {
                return rebindAndIterate(error.params)
              }
              throw error
            })
          }
          return result
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
              const valueRecord = evaluateBindingNodeValues(bindingNode[1][0], asAny(params[index]), Node => evaluateNode(Node, contextStack) as Any)
              for (const [name, value] of Object.entries(valueRecord)) {
                bindingContext[name]!.value = value
              }
            })
            continue
          }
          throw error
        }
      }
    })
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
    const paramsResult = getUndefinedSymbols([node[1][2]], contextStack.create(newContext), builtin, evaluateNode)
    return joinSets(bindingsResult, paramsResult)
  },
}
