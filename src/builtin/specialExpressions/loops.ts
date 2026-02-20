import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateNode } from '../../evaluator/interface'
import type { Any, Arr } from '../../interface'
import type { BindingNode, Node, SpecialExpressionNode } from '../../parser/types'
import { asNonUndefined } from '../../typeGuards'
import { asAny, asColl, isSeq } from '../../typeGuards/lits'
import type { Builtin, BuiltinSpecialExpression, CustomDocs } from '../interface'
import { evaluateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
import type { specialExpressionTypes } from '../specialExpressionTypes'
import { toFixedArity } from '../../utils/arity'

export type LoopBindingNode = [BindingNode, BindingNode[], Node?, Node?] // Binding, Let-Bindings, When, While

export type ForNode = SpecialExpressionNode<[typeof specialExpressionTypes['for'], LoopBindingNode[], Node]> // LoopBindings, body
export type DoSeqNode = SpecialExpressionNode<[typeof specialExpressionTypes['doseq'], LoopBindingNode[], Node]> // LoopBindings, body

type LoopNode = ForNode | DoSeqNode

function addToContext(
  bindings: BindingNode[],
  context: Context,
  contextStack: ContextStack,
  evaluateNode: EvaluateNode,
) {
  for (const bindingNode of bindings) {
    const [target, bindingValue] = bindingNode[1]
    const val = evaluateNode(bindingValue, contextStack)
    const valueRecord = evaluateBindingNodeValues(target, val, Node => evaluateNode(Node, contextStack))
    Object.entries(valueRecord).forEach(([name, value]) => {
      context[name] = { value }
    })
  }
}

function evaluateLoop(
  returnResult: boolean,
  loopNode: LoopNode,
  contextStack: ContextStack,
  evaluateNode: EvaluateNode,
) {
  const sourceCodeInfo = loopNode[2]
  const [, loopBindings, body] = loopNode[1]

  const result: Arr = []

  const bindingIndices = loopBindings.map(() => 0)
  let abort = false
  while (!abort) {
    const context: Context = {}
    const newContextStack = contextStack.create(context)
    let skip = false
    bindingsLoop: for (let bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
      const [bindingNode, letBindings, whenNode, whileNode] = loopBindings[bindingIndex]!
      const [targetNode, valueNode] = bindingNode[1]
      const coll = asColl(evaluateNode(valueNode, newContextStack), sourceCodeInfo)
      const seq = isSeq(coll) ? coll : Object.entries(coll)
      if (seq.length === 0) {
        skip = true
        abort = true
        break
      }
      const index = asNonUndefined(bindingIndices[bindingIndex], sourceCodeInfo)
      if (index >= seq.length) {
        skip = true
        if (bindingIndex === 0) {
          abort = true
          break
        }
        bindingIndices[bindingIndex] = 0
        bindingIndices[bindingIndex - 1] = asNonUndefined(bindingIndices[bindingIndex - 1], sourceCodeInfo) + 1
        break
      }

      const val = asAny(seq[index], sourceCodeInfo)
      const valueRecord = evaluateBindingNodeValues(targetNode, val, Node => evaluateNode(Node, newContextStack))
      Object.entries(valueRecord).forEach(([name, value]) => {
        context[name] = { value }
      })
      if (letBindings) {
        addToContext(
          letBindings,
          context,
          newContextStack,
          evaluateNode,
        )
      }
      if (whenNode && !evaluateNode(whenNode, newContextStack)) {
        bindingIndices[bindingIndex] = asNonUndefined(bindingIndices[bindingIndex], sourceCodeInfo) + 1
        skip = true
        break bindingsLoop
      }
      if (whileNode && !evaluateNode(whileNode, newContextStack)) {
        bindingIndices[bindingIndex] = Number.POSITIVE_INFINITY
        skip = true
        break bindingsLoop
      }
    }
    if (!skip) {
      const value: Any = evaluateNode(body, newContextStack)
      if (returnResult)
        result.push(value)

      if (bindingIndices.length > 0)
        bindingIndices[bindingIndices.length - 1]! += 1
    }
  }

  return returnResult ? result : null
}

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
  evaluate: (node, contextStack, helpers) => evaluateLoop(true, node, contextStack, helpers.evaluateNode),
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
  evaluate: (node, contextStack, helpers) => {
    evaluateLoop(false, node, contextStack, helpers.evaluateNode)
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => analyze(node, contextStack, getUndefinedSymbols, builtin, evaluateNode),
}
