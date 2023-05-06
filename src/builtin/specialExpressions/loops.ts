import { AnalyzeAst, AnalyzeResult } from '../../analyze/interface'
import { LitsError } from '../../errors'
import { Context, ContextStack, EvaluateAstNode } from '../../evaluator/interface'
import { Any, Arr } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { Token, DebugInfo } from '../../tokenizer/interface'
import { any, astNode, asValue, collection, sequence, token } from '../../utils/assertion'
import { Builtin, BuiltinSpecialExpression, ParserHelpers } from '../interface'

type LoopNode = SpecialExpressionNode & {
  loopBindings: LoopBindingNode[]
}

export type LoopBindingNode = {
  binding: BindingNode
  modifiers: Array<`&let` | `&when` | `&while`>
  letBindings?: BindingNode[]
  whenNode?: AstNode
  whileNode?: AstNode
}

function parseLoopBinding(
  tokens: Token[],
  position: number,
  { parseBinding, parseBindings, parseToken }: ParserHelpers,
): [number, LoopBindingNode] {
  let bindingNode: BindingNode
  ;[position, bindingNode] = parseBinding(tokens, position)

  const loopBinding: LoopBindingNode = {
    binding: bindingNode,
    modifiers: [],
  }

  let tkn = token.as(tokens[position], `EOF`)
  while (tkn.type === `modifier`) {
    switch (tkn.value) {
      case `&let`:
        if (loopBinding.letBindings) {
          throw new LitsError(`Only one &let modifier allowed`, tkn.debugInfo)
        }
        ;[position, loopBinding.letBindings] = parseBindings(tokens, position + 1)
        loopBinding.modifiers.push(`&let`)
        break
      case `&when`:
        if (loopBinding.whenNode) {
          throw new LitsError(`Only one &when modifier allowed`, tkn.debugInfo)
        }
        ;[position, loopBinding.whenNode] = parseToken(tokens, position + 1)
        loopBinding.modifiers.push(`&when`)
        break
      case `&while`:
        if (loopBinding.whileNode) {
          throw new LitsError(`Only one &while modifier allowed`, tkn.debugInfo)
        }
        ;[position, loopBinding.whileNode] = parseToken(tokens, position + 1)
        loopBinding.modifiers.push(`&while`)
        break
      default:
        throw new LitsError(`Illegal modifier: ${tkn.value}`, tkn.debugInfo)
    }
    tkn = token.as(tokens[position], `EOF`)
  }
  return [position, loopBinding]
}

function addToContext(
  bindings: BindingNode[],
  context: Context,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
  debugInfo?: DebugInfo,
) {
  for (const binding of bindings) {
    if (context[binding.name]) {
      throw new LitsError(`Variable already defined: ${binding.name}.`, debugInfo)
    }
    context[binding.name] = { value: evaluateAstNode(binding.value, contextStack) }
  }
}

function parseLoopBindings(tokens: Token[], position: number, parsers: ParserHelpers): [number, LoopBindingNode[]] {
  token.assert(tokens[position], `EOF`, { type: `paren`, value: `[` })
  position += 1

  const loopBindings: LoopBindingNode[] = []

  let tkn = token.as(tokens[position], `EOF`)
  while (!token.is(tkn, { type: `paren`, value: `]` })) {
    let loopBinding: LoopBindingNode
    ;[position, loopBinding] = parseLoopBinding(tokens, position, parsers)
    loopBindings.push(loopBinding)
    tkn = token.as(tokens[position], `EOF`)
  }
  return [position + 1, loopBindings]
}
function evaluateLoop(
  returnResult: boolean,
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
) {
  const debugInfo = node.token?.debugInfo
  const { loopBindings, params } = node as LoopNode
  const expression = astNode.as(params[0], debugInfo)

  const result: Arr = []

  const bindingIndices = loopBindings.map(() => 0)
  let abort = false
  while (!abort) {
    const context: Context = {}
    const newContextStack = contextStack.withContext(context)
    let skip = false
    bindingsLoop: for (let bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
      const { binding, letBindings, whenNode, whileNode, modifiers } = asValue(loopBindings[bindingIndex], debugInfo)
      const coll = collection.as(evaluateAstNode(binding.value, newContextStack), debugInfo)
      const seq = sequence.is(coll) ? coll : Object.entries(coll)
      if (seq.length === 0) {
        skip = true
        abort = true
        break
      }
      const index = asValue(bindingIndices[bindingIndex], debugInfo)
      if (index >= seq.length) {
        skip = true
        if (bindingIndex === 0) {
          abort = true
          break
        }
        bindingIndices[bindingIndex] = 0
        bindingIndices[bindingIndex - 1] = asValue(bindingIndices[bindingIndex - 1], debugInfo) + 1
        break
      }
      if (context[binding.name]) {
        throw new LitsError(`Variable already defined: ${binding.name}.`, debugInfo)
      }
      context[binding.name] = {
        value: any.as(seq[index], debugInfo),
      }
      for (const modifier of modifiers) {
        switch (modifier) {
          case `&let`:
            addToContext(asValue(letBindings, debugInfo), context, newContextStack, evaluateAstNode, debugInfo)
            break
          case `&when`:
            if (!evaluateAstNode(astNode.as(whenNode, debugInfo), newContextStack)) {
              bindingIndices[bindingIndex] = asValue(bindingIndices[bindingIndex], debugInfo) + 1
              skip = true
              break bindingsLoop
            }
            break
          case `&while`:
            if (!evaluateAstNode(astNode.as(whileNode, debugInfo), newContextStack)) {
              bindingIndices[bindingIndex] = Number.POSITIVE_INFINITY
              skip = true
              break bindingsLoop
            }
            break
        }
      }
    }
    if (!skip) {
      const value = evaluateAstNode(expression, newContextStack)
      if (returnResult) {
        result.push(value)
      }
      bindingIndices[bindingIndices.length - 1] += 1
    }
  }
  return returnResult ? result : null
}

function analyze(
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  analyzeAst: AnalyzeAst,
  builtin: Builtin,
): AnalyzeResult {
  const result: AnalyzeResult = {
    undefinedSymbols: new Set(),
  }
  const newContext: Context = {}
  const { loopBindings } = node as LoopNode
  loopBindings.forEach(loopBinding => {
    const { binding, letBindings, whenNode, whileNode } = loopBinding
    analyzeAst(binding.value, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(symbol =>
      result.undefinedSymbols.add(symbol),
    )
    newContext[binding.name] = { value: true }
    if (letBindings) {
      letBindings.forEach(letBinding => {
        analyzeAst(letBinding.value, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(symbol =>
          result.undefinedSymbols.add(symbol),
        )
        newContext[letBinding.name] = { value: true }
      })
    }
    if (whenNode) {
      analyzeAst(whenNode, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(symbol =>
        result.undefinedSymbols.add(symbol),
      )
    }
    if (whileNode) {
      analyzeAst(whileNode, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(symbol =>
        result.undefinedSymbols.add(symbol),
      )
    }
  })
  analyzeAst(node.params, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(symbol =>
    result.undefinedSymbols.add(symbol),
  )
  return result
}

export const forSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens: Token[], position: number, parsers: ParserHelpers) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const { parseToken } = parsers
    let loopBindings: LoopBindingNode[]
    ;[position, loopBindings] = parseLoopBindings(tokens, position, parsers)

    let expression: AstNode
    ;[position, expression] = parseToken(tokens, position)

    token.assert(tokens[position], `EOF`, { type: `paren`, value: `)` })

    const node: LoopNode = {
      name: `for`,
      type: `SpecialExpression`,
      loopBindings,
      params: [expression],
      token: firstToken.debugInfo ? firstToken : undefined,
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack, helpers) => evaluateLoop(true, node, contextStack, helpers.evaluateAstNode),
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyze(node, contextStack, analyzeAst, builtin),
}

export const doseqSpecialExpression: BuiltinSpecialExpression<null> = {
  parse: (tokens: Token[], position: number, parsers: ParserHelpers) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const { parseToken } = parsers
    let loopBindings: LoopBindingNode[]
    ;[position, loopBindings] = parseLoopBindings(tokens, position, parsers)

    let expression: AstNode
    ;[position, expression] = parseToken(tokens, position)

    token.assert(tokens[position], `EOF`, { type: `paren`, value: `)` })

    const node: LoopNode = {
      name: `doseq`,
      type: `SpecialExpression`,
      loopBindings,
      params: [expression],
      token: firstToken.debugInfo ? firstToken : undefined,
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack, helpers) => {
    evaluateLoop(false, node, contextStack, helpers.evaluateAstNode)
    return null
  },
  analyze: (node, contextStack, { analyzeAst, builtin }) => analyze(node, contextStack, analyzeAst, builtin),
}
