import type { Any } from '../../interface'
import type { AstNode, BindingTarget, SpecialExpressionNode } from '../../parser/types'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'
import { getAllBindingTargetNames } from '../bindingNode'
import type { Context } from '../../evaluator/interface'

// Each case: [pattern, body, guard?]
export type MatchCase = [BindingTarget, AstNode, AstNode | undefined]
export type MatchNode = SpecialExpressionNode<[typeof specialExpressionTypes['match'], AstNode, MatchCase[]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['match value match-branch match-branch ... end'],
  details: [
    ['value', 'any', 'The value to match against patterns.'],
    ['match-branch', 'case pattern [when guard] then body', 'A branch of the match expression.'],
    ['pattern', 'pattern', 'A pattern to match: literal, variable, array destructuring, object destructuring, or wildcard (_).'],
    ['guard', 'expression', 'An optional guard expression that must be truthy for the match to succeed.'],
    ['body', 'expressions', 'The expressions to evaluate if the pattern matches.'],
  ],
  description: 'Pattern matching expression. Matches `value` against each `pattern` sequentially. If a pattern matches (and the optional `when` guard is truthy), the corresponding `body` is evaluated and its result returned. Bound variables from the pattern are available in the guard and body. If no pattern matches, `null` is returned.',
  examples: [
    `
match 1
  case 1 then "One"
  case 2 then "Two"
end`,
    `
match [1, 2, 3]
  case [x] then "one element"
  case [x, y] then "two elements"
  case [x, ...xs] then "first: " ++ str(x) ++ " rest: " ++ str(xs)
end`,
    `
match { type: "click", x: 10, y: 20 }
  case { type: "click", x, y } then "Click at " ++ str(x) ++ ", " ++ str(y)
  case { type: "keydown", key } then "Key: " ++ key
  case _ then "unknown event"
end`,
    `
match { role: "admin", name: "Alice" }
  case { role: "admin", name } then "Admin: " ++ name
  case { role, name } when role == "user" then "User: " ++ name
  case _ then "Unknown role"
end`,
  ],
}

export const matchSpecialExpression: BuiltinSpecialExpression<Any, MatchNode> = {
  arity: {},
  docs,
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const result = new Set<string>()

    // The match value expression
    getUndefinedSymbols([node[1][1]], contextStack, builtin, evaluateNode).forEach(s => result.add(s))

    // Each case
    for (const [pattern, body, guard] of node[1][2]) {
      const newContext: Context = {}
      Object.assign(newContext, getAllBindingTargetNames(pattern))
      const caseContextStack = contextStack.create(newContext)

      if (guard) {
        getUndefinedSymbols([guard], caseContextStack, builtin, evaluateNode).forEach(s => result.add(s))
      }
      getUndefinedSymbols([body], caseContextStack, builtin, evaluateNode).forEach(s => result.add(s))
    }

    return result
  },
}
