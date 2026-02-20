import type { Any } from '../../interface'
import type { Node, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type SwitchNode = SpecialExpressionNode<[typeof specialExpressionTypes['switch'], Node, [Node, Node][]]>

const docs: CustomDocs = {
  category: 'Special-Expression',
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
    const switchValue = evaluateNode(switchValueNode, contextStack)
    for (const [test, form] of cases) {
      const value = evaluateNode(test, contextStack)
      if (value === switchValue) {
        return evaluateNode(form, contextStack)
      }
    }
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => getUndefinedSymbols([node[1][1], ...node[1][2].flat()], contextStack, builtin, evaluateNode),
}
