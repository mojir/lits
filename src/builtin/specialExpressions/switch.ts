import type { Any } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import type { MaybePromise } from '../../utils/maybePromise'
import { chain } from '../../utils/maybePromise'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type SwitchNode = SpecialExpressionNode<[typeof specialExpressionTypes['switch'], AstNode, [AstNode, AstNode][]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['switch value switch-branch switch-branch ... end'],
  details: [
    ['value', 'any', 'The value to test.'],
    ['switch-branch', 'case test then body', 'A branch of the switch expression.'],
    ['test', 'expression', 'The condition to test.'],
    ['body', 'expressions', 'The expressions to evaluate if the test is truthy.'],
  ],
  description: 'Used for branching. `switch-branches` are tested sequentially from the top against `value`. If no branch is tested truthy, `null` is returned.',
  examples: [
    `
switch 1
  case 1 then write!("One")
  case 2 then write!("Two")
end`,
    `
switch 2
  case 1 then write!("One")
  case 2 then write!("Two")
end`,
    `
switch 3
  case 1 then write!("One")
  case 2 then write!("Two")
end`,
  ],
}

export const switchSpecialExpression: BuiltinSpecialExpression<Any, SwitchNode> = {
  arity: {},
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const [, switchValueNode, cases] = node[1]
    return chain(evaluateNode(switchValueNode, contextStack), (switchValue) => {
      function processCase(index: number): MaybePromise<Any> {
        if (index >= cases.length)
          return null
        const [test, form] = cases[index]!
        return chain(evaluateNode(test, contextStack), (value) => {
          if (value === switchValue) {
            return evaluateNode(form, contextStack)
          }
          return processCase(index + 1)
        })
      }
      return processCase(0)
    })
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols([node[1][1], ...node[1][2].flat()], contextStack, builtin, evaluateNode),
}
