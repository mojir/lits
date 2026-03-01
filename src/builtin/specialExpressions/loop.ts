import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/types'
import { joinSets } from '../../utils'
import { getAllBindingTargetNames } from '../bindingNode'
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
