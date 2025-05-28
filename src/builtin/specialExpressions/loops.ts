import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateNode } from '../../evaluator/interface'
import type { Any, Arr } from '../../interface'
import type { BindingNode, Node, SpecialExpressionNode } from '../../parser/types'
import { asNonUndefined } from '../../typeGuards'
import { asAny, asColl, isSeq } from '../../typeGuards/lits'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import { evalueateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'
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
    const valueRecord = evalueateBindingNodeValues(target, val, Node => evaluateNode(Node, contextStack))
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
      const valueRecord = evalueateBindingNodeValues(targetNode, val, Node => evaluateNode(Node, newContextStack))
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

export const forSpecialExpression: BuiltinSpecialExpression<Any, ForNode> = {
  arity: toFixedArity(1),
  evaluate: (node, contextStack, helpers) => evaluateLoop(true, node, contextStack, helpers.evaluateNode),
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => analyze(node, contextStack, getUndefinedSymbols, builtin, evaluateNode),
}

export const doseqSpecialExpression: BuiltinSpecialExpression<null, DoSeqNode> = {
  arity: toFixedArity(1),
  evaluate: (node, contextStack, helpers) => {
    evaluateLoop(false, node, contextStack, helpers.evaluateNode)
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => analyze(node, contextStack, getUndefinedSymbols, builtin, evaluateNode),
}
