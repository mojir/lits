import { LitsError, UnexpectedTokenError } from '../../errors'
import { Context, ContextStack, EvaluateAstNode } from '../../evaluator/interface'
import { Any, Arr } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { Token, TokenMeta } from '../../tokenizer/interface'
import { asNotUndefined } from '../../utils'
import { any, collection, sequence } from '../../utils/assertion'
import { BuiltinSpecialExpression, Parsers } from '../interface'

interface ForSpecialExpressionNode extends SpecialExpressionNode {
  name: `for`
  loopBindings: LoopBindingNode[]
}

type LoopBindingNode = {
  binding: BindingNode
  modifiers: Array<`&let` | `&when` | `&while`>
  letBindings?: BindingNode[]
  whenNode?: AstNode
  whileNode?: AstNode
}

function parseLoopBinding(
  tokens: Token[],
  position: number,
  { parseBinding, parseBindings, parseToken }: Parsers,
): [number, LoopBindingNode] {
  let bindingNode: BindingNode
  ;[position, bindingNode] = parseBinding(tokens, position)

  const loopBinding: LoopBindingNode = {
    binding: bindingNode,
    modifiers: [],
  }

  let token = asNotUndefined(tokens[position], `EOF`)
  while (token.type === `modifier`) {
    switch (token.value) {
      case `&let`:
        if (loopBinding.letBindings) {
          throw new LitsError(`Only one &let modifier allowed`, token.meta)
        }
        ;[position, loopBinding.letBindings] = parseBindings(tokens, position + 1)
        loopBinding.modifiers.push(`&let`)
        break
      case `&when`:
        if (loopBinding.whenNode) {
          throw new LitsError(`Only one &when modifier allowed`, token.meta)
        }
        ;[position, loopBinding.whenNode] = parseToken(tokens, position + 1)
        loopBinding.modifiers.push(`&when`)
        break
      case `&while`:
        if (loopBinding.whileNode) {
          throw new LitsError(`Only one &while modifier allowed`, token.meta)
        }
        ;[position, loopBinding.whileNode] = parseToken(tokens, position + 1)
        loopBinding.modifiers.push(`&while`)
        break
      default:
        throw new LitsError(`Illegal modifier: ${token.value}`, token.meta)
    }
    token = asNotUndefined(tokens[position], `EOF`)
  }
  return [position, loopBinding]
}

function addToContext(
  bindings: BindingNode[],
  context: Context,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
  meta: TokenMeta,
) {
  for (const binding of bindings) {
    if (context[binding.name]) {
      throw new LitsError(`Variable already defined: ${binding.name}`, meta)
    }
    context[binding.name] = { value: evaluateAstNode(binding.value, contextStack) }
  }
}

function parseLoopBindings(tokens: Token[], position: number, parsers: Parsers): [number, LoopBindingNode[]] {
  let token = asNotUndefined(tokens[position], `EOF`)
  if (!(token.type === `paren` && token.value === `[`)) {
    throw new UnexpectedTokenError(`[`, token)
  }
  position += 1

  const loopBindings: LoopBindingNode[] = []

  token = asNotUndefined(tokens[position], `EOF`)
  while (!(token.type === `paren` && token.value === `]`)) {
    let loopBinding: LoopBindingNode
    ;[position, loopBinding] = parseLoopBinding(tokens, position, parsers)
    loopBindings.push(loopBinding)
    token = asNotUndefined(tokens[position], `EOF`)
  }
  return [position + 1, loopBindings]
}

export const forSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, parsers) => {
    const firstToken = asNotUndefined(tokens[position], `EOF`)
    const { parseToken } = parsers
    let loopBindings: LoopBindingNode[]
    ;[position, loopBindings] = parseLoopBindings(tokens, position, parsers)

    let expression: AstNode
    ;[position, expression] = parseToken(tokens, position)

    const token = asNotUndefined(tokens[position], `EOF`)
    if (!(token.type === `paren` && token.value === `)`)) {
      throw new UnexpectedTokenError(`)`, token)
    }

    const node: ForSpecialExpressionNode = {
      name: `for`,
      type: `SpecialExpression`,
      loopBindings,
      params: [expression],
      token: firstToken,
    }

    return [position + 1, node]
  },
  evaluate: (node, contextStack, { evaluateAstNode }) => {
    castLoopExpressionNode(node)
    const meta = node.token.meta
    const { loopBindings, params } = node
    const expression = asNotUndefined(params[0], meta)

    const result: Arr = []

    const bindingIndices = loopBindings.map(() => 0)
    let abort = false
    while (!abort) {
      const context: Context = {}
      const newContextStack = contextStack.withContext(context)
      let skip = false
      bindingsLoop: for (let bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
        const { binding, letBindings, whenNode, whileNode, modifiers } = asNotUndefined(
          loopBindings[bindingIndex],
          meta,
        )
        const coll = collection.as(evaluateAstNode(binding.value, newContextStack), meta)
        const seq = sequence.is(coll) ? coll : Object.entries(coll)
        if (seq.length === 0) {
          skip = true
          abort = true
          break
        }
        const index = asNotUndefined(bindingIndices[bindingIndex], meta)
        if (index >= seq.length) {
          skip = true
          if (bindingIndex === 0) {
            abort = true
            break
          }
          bindingIndices[bindingIndex] = 0
          bindingIndices[bindingIndex - 1] = asNotUndefined(bindingIndices[bindingIndex - 1], meta) + 1
          break
        }
        if (context[binding.name]) {
          throw new LitsError(`Variable already defined: ${binding.name}`, meta)
        }
        context[binding.name] = {
          value: any.as(seq[index], meta),
        }
        for (const modifier of modifiers) {
          switch (modifier) {
            case `&let`:
              addToContext(asNotUndefined(letBindings, meta), context, newContextStack, evaluateAstNode, meta)
              break
            case `&when`:
              if (!evaluateAstNode(asNotUndefined(whenNode, meta), newContextStack)) {
                bindingIndices[bindingIndex] = asNotUndefined(bindingIndices[bindingIndex], meta) + 1
                skip = true
                break bindingsLoop
              }
              break
            case `&while`:
              if (!evaluateAstNode(asNotUndefined(whileNode, meta), newContextStack)) {
                bindingIndices[bindingIndex] = Number.POSITIVE_INFINITY
                skip = true
                break bindingsLoop
              }
              break
          }
        }
      }
      if (!skip) {
        result.push(evaluateAstNode(expression, newContextStack))
        bindingIndices[bindingIndices.length - 1] += 1
      }
    }
    return result
  },
}

function castLoopExpressionNode(_node: SpecialExpressionNode): asserts _node is ForSpecialExpressionNode {
  return
}
