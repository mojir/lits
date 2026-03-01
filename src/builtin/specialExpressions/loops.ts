import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateNode } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/types'
import type { Builtin, BuiltinSpecialExpression, CustomDocs } from '../interface'
import { getAllBindingTargetNames } from '../bindingNode'
import type { specialExpressionTypes } from '../specialExpressionTypes'
import { toFixedArity } from '../../utils/arity'

export type LoopBindingNode = [BindingNode, BindingNode[], AstNode?, AstNode?] // Binding, Let-Bindings, When, While

export type ForNode = SpecialExpressionNode<[typeof specialExpressionTypes['for'], LoopBindingNode[], AstNode]> // LoopBindings, body
export type DoSeqNode = SpecialExpressionNode<[typeof specialExpressionTypes['doseq'], LoopBindingNode[], AstNode]> // LoopBindings, body

type LoopNode = ForNode | DoSeqNode

function analyze(
  loopNode: LoopNode,
  contextStack: ContextStack,
  getUndefinedSymbols: GetUndefinedSymbols,
  builtin: Builtin,
  evaluateNode: EvaluateNode,
): UndefinedSymbols {
  const result = new Set<string>()
  const newContext: Context = {}
  const [, loopBindings, body] = loopNode[1]
  loopBindings.forEach((loopBindingNode) => {
    const [bindingNode, letBindings, whenNode, whileNode] = loopBindingNode
    const [target, value] = bindingNode[1]
    getUndefinedSymbols([value], contextStack.create(newContext), builtin, evaluateNode).forEach(symbol =>
      result.add(symbol),
    )
    Object.assign(newContext, getAllBindingTargetNames(target))
    if (letBindings) {
      letBindings.forEach((letBindingNode) => {
        const [letTarget, letValue] = letBindingNode[1]

        getUndefinedSymbols([letValue], contextStack.create(newContext), builtin, evaluateNode).forEach(symbol =>
          result.add(symbol),
        )
        Object.assign(newContext, getAllBindingTargetNames(letTarget))
      })
    }
    if (whenNode) {
      getUndefinedSymbols([whenNode], contextStack.create(newContext), builtin, evaluateNode).forEach(symbol =>
        result.add(symbol),
      )
    }
    if (whileNode) {
      getUndefinedSymbols([whileNode], contextStack.create(newContext), builtin, evaluateNode).forEach(symbol =>
        result.add(symbol),
      )
    }
  })
  getUndefinedSymbols([body], contextStack.create(newContext), builtin, evaluateNode).forEach(symbol =>
    result.add(symbol),
  )
  return result
}

const forDocs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['for (...binding) -> body'],
  details: [
    ['binding', 'loop-var in collection [...let-binding] [where whereExpr] [while whileExp]', 'A for loop binding'],
    ['loop-var', 'symbol', 'The name of the loop variable.'],
    ['collection', 'any', 'The collection to iterate over.'],
    ['let-binding', 'let binding', 'A let binding to create a local variable.'],
    ['whereExpr', 'expression', 'An expression that must evaluate to truthy for the loop body to be executed.'],
    ['whileExp', 'expression', 'An expression that must evaluate to truthy for the loop to continue.'],
    ['body', 'expressions', 'The expressions to evaluate for each iteration of the loop.'],
  ],
  returns: {
    type: 'any',
    array: true,
  },
  description: 'Iterates over `bindings`, evaluates `body` for each `binding` and returns an `array` of results.',
  examples: [
    `
for (i in [1, 2, 3]) -> i * 2
      `,
    `
for (
  i in range(10) let ii = i ^ 2 while ii < 40 when ii % 3 == 0,
  j in range(10) when j % 2 == 1
) -> ii + j
      `,
  ],
}

export const forSpecialExpression: BuiltinSpecialExpression<Any, ForNode> = {
  arity: toFixedArity(1),
  docs: forDocs,
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => analyze(node, contextStack, getUndefinedSymbols, builtin, evaluateNode),
}

const doseqDocs: CustomDocs = {
  category: 'special-expression',
  customVariants: ['doseq (...binding) -> body'],
  details: [
    ['binding', 'loop-var in collection [...let-binding] [where whereExpr] [while whileExp]', 'A doseq loop binding'],
    ['loop-var', 'symbol', 'The name of the loop variable.'],
    ['collection', 'any', 'The collection to iterate over.'],
    ['let-binding', 'let binding', 'A let binding to create a local variable.'],
    ['whereExpr', 'expression', 'An expression that must evaluate to truthy for the loop body to be executed.'],
    ['whileExp', 'expression', 'An expression that must evaluate to truthy for the loop to continue.'],
    ['body', 'expressions', 'The expressions to evaluate for each iteration of the loop.'],
  ],
  returns: {
    type: 'null',
  },
  description: 'Iterates over `bindings`, evaluates `body` for each `binding` and returns `null`. This is useful for side effects.',
  examples: [
    `
doseq (i in [1, 2, 3]) -> write!(i * 2)
      `,
  ],
}

export const doseqSpecialExpression: BuiltinSpecialExpression<null, DoSeqNode> = {
  arity: toFixedArity(1),
  docs: doseqDocs,
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => analyze(node, contextStack, getUndefinedSymbols, builtin, evaluateNode),
}
