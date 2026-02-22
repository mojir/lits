import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateNode } from '../../evaluator/interface'
import type { Any, Arr } from '../../interface'
import type { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/types'
import { asNonUndefined } from '../../typeGuards'
import { asAny, asColl, isSeq } from '../../typeGuards/lits'
import type { Builtin, BuiltinSpecialExpression, CustomDocs } from '../interface'
import { evaluateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
import type { specialExpressionTypes } from '../specialExpressionTypes'
import { toFixedArity } from '../../utils/arity'
import type { MaybePromise } from '../../utils/maybePromise'
import { chain } from '../../utils/maybePromise'

export type LoopBindingNode = [BindingNode, BindingNode[], AstNode?, AstNode?] // Binding, Let-Bindings, When, While

export type ForNode = SpecialExpressionNode<[typeof specialExpressionTypes['for'], LoopBindingNode[], AstNode]> // LoopBindings, body
export type DoSeqNode = SpecialExpressionNode<[typeof specialExpressionTypes['doseq'], LoopBindingNode[], AstNode]> // LoopBindings, body

type LoopNode = ForNode | DoSeqNode

function addToContext(
  bindings: BindingNode[],
  context: Context,
  contextStack: ContextStack,
  evaluateNode: EvaluateNode,
): MaybePromise<void> {
  let bindingChain: MaybePromise<void> = undefined as unknown as void
  for (const bindingNode of bindings) {
    bindingChain = chain(bindingChain, () => {
      const [target, bindingValue] = bindingNode[1]
      return chain(evaluateNode(bindingValue, contextStack), (val) => {
        return chain(evaluateBindingNodeValues(target, val, Node => evaluateNode(Node, contextStack)), (valueRecord) => {
          Object.entries(valueRecord).forEach(([name, value]) => {
            context[name] = { value }
          })
        })
      })
    })
  }
  return bindingChain
}

function evaluateLoop(
  returnResult: boolean,
  loopNode: LoopNode,
  contextStack: ContextStack,
  evaluateNode: EvaluateNode,
): MaybePromise<Any> {
  const sourceCodeInfo = loopNode[2]
  const [, loopBindings, body] = loopNode[1]

  const result: Arr = []

  const bindingIndices = loopBindings.map(() => 0)

  function processIteration(): MaybePromise<Any> {
    const context: Context = {}
    const newContextStack = contextStack.create(context)

    function processBinding(bindingIndex: number): MaybePromise<'skip' | 'abort' | 'continue'> {
      if (bindingIndex >= loopBindings.length)
        return 'continue'

      const [bindingNode, letBindings, whenNode, whileNode] = loopBindings[bindingIndex]!
      const [targetNode, valueNode] = bindingNode[1]

      return chain(evaluateNode(valueNode, newContextStack), (rawColl) => {
        const coll = asColl(rawColl, sourceCodeInfo)
        const seq = isSeq(coll) ? coll : Object.entries(coll)

        if (seq.length === 0) {
          return 'abort'
        }

        const index = asNonUndefined(bindingIndices[bindingIndex], sourceCodeInfo)
        if (index >= seq.length) {
          if (bindingIndex === 0) {
            return 'abort'
          }
          bindingIndices[bindingIndex] = 0
          bindingIndices[bindingIndex - 1] = asNonUndefined(bindingIndices[bindingIndex - 1], sourceCodeInfo) + 1
          return 'skip'
        }

        const val = asAny(seq[index], sourceCodeInfo)
        return chain(evaluateBindingNodeValues(targetNode, val, Node => evaluateNode(Node, newContextStack)), (valueRecord) => {
          Object.entries(valueRecord).forEach(([name, value]) => {
            context[name] = { value }
          })

          return chain(
            letBindings.length > 0
              ? addToContext(letBindings, context, newContextStack, evaluateNode)
              : undefined as unknown as void,
            () => {
              if (whenNode) {
                return chain(evaluateNode(whenNode, newContextStack), (whenResult) => {
                  if (!whenResult) {
                    bindingIndices[bindingIndex] = asNonUndefined(bindingIndices[bindingIndex], sourceCodeInfo) + 1
                    return 'skip' as const
                  }
                  if (whileNode) {
                    return chain(evaluateNode(whileNode, newContextStack), (whileResult) => {
                      if (!whileResult) {
                        bindingIndices[bindingIndex] = Number.POSITIVE_INFINITY
                        return 'skip' as const
                      }
                      return processBinding(bindingIndex + 1)
                    })
                  }
                  return processBinding(bindingIndex + 1)
                })
              }
              if (whileNode) {
                return chain(evaluateNode(whileNode, newContextStack), (whileResult) => {
                  if (!whileResult) {
                    bindingIndices[bindingIndex] = Number.POSITIVE_INFINITY
                    return 'skip' as const
                  }
                  return processBinding(bindingIndex + 1)
                })
              }
              return processBinding(bindingIndex + 1)
            },
          )
        })
      })
    }

    return chain(processBinding(0), (status) => {
      if (status === 'abort') {
        return returnResult ? result : null
      }
      if (status === 'skip') {
        return processIteration()
      }
      // status === 'continue'
      return chain(evaluateNode(body, newContextStack), (value) => {
        if (returnResult)
          result.push(value)

        if (bindingIndices.length > 0)
          bindingIndices[bindingIndices.length - 1]! += 1

        return processIteration()
      })
    })
  }

  return processIteration()
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
    return chain(evaluateLoop(false, node, contextStack, helpers.evaluateNode), () => null)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => analyze(node, contextStack, getUndefinedSymbols, builtin, evaluateNode),
}
