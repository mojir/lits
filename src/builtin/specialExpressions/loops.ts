import type { SpecialExpressionNode } from '..'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../../getUndefinedSymbols'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateAstNode } from '../../evaluator/interface'
import type { Any, Arr } from '../../interface'
import type { AstNode, BindingNode, CommonSpecialExpressionNode } from '../../parser/types'
import { asNonUndefined } from '../../typeGuards'
import { asAstNode } from '../../typeGuards/astNode'
import { asAny, asColl, isSeq } from '../../typeGuards/lits'
import type { Builtin, BuiltinSpecialExpression } from '../interface'
import { evalueateBindingNodeValues, getAllBindingTargetNames } from '../bindingNode'

export interface ForNode extends CommonSpecialExpressionNode<'for'> {
  l: LoopBindingNode[]
}

export interface DoSeqNode extends CommonSpecialExpressionNode<'doseq'> {
  l: LoopBindingNode[]
}

type LoopNode = ForNode | DoSeqNode

export interface LoopBindingNode {
  b: BindingNode // Binding
  m: Array<'&let' | '&when' | '&while'> // Modifiers
  l?: BindingNode[] // Let-Bindings
  wn?: AstNode // When Node
  we?: AstNode // While Node
}

function addToContext(
  bindings: BindingNode[],
  context: Context,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
) {
  for (const binding of bindings) {
    const val = evaluateAstNode(binding.value, contextStack)
    const valueRecord
    = evalueateBindingNodeValues(binding, val, astNode => evaluateAstNode(astNode, contextStack))
    Object.entries(valueRecord).forEach(([name, value]) => {
      context[name] = { value }
    })
  }
}

function evaluateLoop(
  returnResult: boolean,
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
) {
  const sourceCodeInfo = node.sourceCodeInfo
  const { l: loopBindings, params } = node as LoopNode

  const result: Arr = []

  const bindingIndices = loopBindings.map(() => 0)
  let abort = false
  while (!abort) {
    const context: Context = {}
    const newContextStack = contextStack.create(context)
    let skip = false
    bindingsLoop: for (let bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
      const {
        b: binding,
        l: letBindings,
        wn: whenNode,
        we: whileNode,
        m: modifiers,
      } = asNonUndefined(loopBindings[bindingIndex], sourceCodeInfo)
      const coll = asColl(evaluateAstNode(binding.value, newContextStack), sourceCodeInfo)
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
      const valueRecord = evalueateBindingNodeValues(binding, val, astNode => evaluateAstNode(astNode, newContextStack))
      Object.entries(valueRecord).forEach(([name, value]) => {
        context[name] = { value }
      })
      for (const modifier of modifiers) {
        switch (modifier) {
          case '&let':
            addToContext(
              asNonUndefined(letBindings, sourceCodeInfo),
              context,
              newContextStack,
              evaluateAstNode,
            )
            break
          case '&when':
            if (!evaluateAstNode(asAstNode(whenNode, sourceCodeInfo), newContextStack)) {
              bindingIndices[bindingIndex] = asNonUndefined(bindingIndices[bindingIndex], sourceCodeInfo) + 1
              skip = true
              break bindingsLoop
            }
            break
          case '&while':
            if (!evaluateAstNode(asAstNode(whileNode, sourceCodeInfo), newContextStack)) {
              bindingIndices[bindingIndex] = Number.POSITIVE_INFINITY
              skip = true
              break bindingsLoop
            }
            break
        }
      }
    }
    if (!skip) {
      const value = evaluateAstNode(params[0]!, newContextStack)
      if (returnResult)
        result.push(value)

      if (bindingIndices.length > 0)
        bindingIndices[bindingIndices.length - 1]! += 1
    }
  }
  return returnResult ? result : null
}

function analyze(
  node: LoopNode,
  contextStack: ContextStack,
  getUndefinedSymbols: GetUndefinedSymbols,
  builtin: Builtin,
  evaluateAstNode: EvaluateAstNode,
): UndefinedSymbols {
  const result = new Set<string>()
  const newContext: Context = {}
  const { l: loopBindings } = node
  loopBindings.forEach((loopBinding) => {
    const { b: binding, l: letBindings, wn: whenNode, we: whileNode } = loopBinding
    getUndefinedSymbols([binding.value], contextStack.create(newContext), builtin, evaluateAstNode).forEach(symbol =>
      result.add(symbol),
    )
    Object.assign(newContext, getAllBindingTargetNames(binding.target))
    if (letBindings) {
      letBindings.forEach((letBinding) => {
        getUndefinedSymbols([letBinding.value], contextStack.create(newContext), builtin, evaluateAstNode).forEach(symbol =>
          result.add(symbol),
        )
        Object.assign(newContext, getAllBindingTargetNames(letBinding.target))
      })
    }
    if (whenNode) {
      getUndefinedSymbols([whenNode], contextStack.create(newContext), builtin, evaluateAstNode).forEach(symbol =>
        result.add(symbol),
      )
    }
    if (whileNode) {
      getUndefinedSymbols([whileNode], contextStack.create(newContext), builtin, evaluateAstNode).forEach(symbol =>
        result.add(symbol),
      )
    }
  })
  getUndefinedSymbols(node.params, contextStack.create(newContext), builtin, evaluateAstNode).forEach(symbol =>
    result.add(symbol),
  )
  return result
}

export const forSpecialExpression: BuiltinSpecialExpression<Any, ForNode> = {
  paramCount: 1,
  evaluate: (node, contextStack, helpers) => evaluateLoop(true, node, contextStack, helpers.evaluateAstNode),
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => analyze(node, contextStack, getUndefinedSymbols, builtin, evaluateAstNode),
}

export const doseqSpecialExpression: BuiltinSpecialExpression<null, DoSeqNode> = {
  paramCount: 1,
  evaluate: (node, contextStack, helpers) => {
    evaluateLoop(false, node, contextStack, helpers.evaluateAstNode)
    return null
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateAstNode }) => analyze(node, contextStack, getUndefinedSymbols, builtin, evaluateAstNode),
}
