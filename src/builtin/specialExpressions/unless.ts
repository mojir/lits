import type { Any } from '../../interface'
import type { AstNode, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type UnlessNode = SpecialExpressionNode<[typeof specialExpressionTypes['unless'], [AstNode, AstNode, AstNode?]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['unless test then true-expr else false-expr end', 'unless test true-expr end'],
  details: [
    ['test', 'expression', 'The condition to test.'],
    ['true-expr', 'expression', 'The expressions to evaluate if the test is falsy.'],
    ['false-expr', 'expression', 'The expressions to evaluate if the test is truthy.'],
  ],
  description: 'Either `true-expr` or `false-expr` branch is taken. `true-expr` is selected when $test is falsy. If $test is truthy `false-expr` is executed, if no `false-expr` exists, `null` is returned.',
  examples: [
    `
unless true then
  write!("TRUE")
else
  write!("FALSE")
end`,
    'unless false then write!("TRUE") else write!("FALSE") end',
    'unless true then write!("TRUE") end',
    'unless false then write!("TRUE") end',
  ],
}

export const unlessSpecialExpression: BuiltinSpecialExpression<Any, UnlessNode> = {
  arity: {},
  docs,
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) =>
    getUndefinedSymbols(node[1][1].filter(n => !!n), contextStack, builtin, evaluateNode),
}
