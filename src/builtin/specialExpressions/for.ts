import { UnexpectedTokenError } from '../../errors'
import { Context, ContextStack, EvaluateAstNode } from '../../evaluator/interface'
import { Any, Arr } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { Token } from '../../tokenizer/interface'
import { asAny, asColl, asNotUndefined, isSeq } from '../../utils'
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

  let token = asNotUndefined(tokens[position])
  while (token.type === `modifier`) {
    switch (token.value) {
      case `&let`:
        if (loopBinding.letBindings) {
          throw Error(`Only one &let modifier allowed`)
        }
        ;[position, loopBinding.letBindings] = parseBindings(tokens, position + 1)
        loopBinding.modifiers.push(`&let`)
        break
      case `&when`:
        if (loopBinding.whenNode) {
          throw Error(`Only one &when modifier allowed`)
        }
        ;[position, loopBinding.whenNode] = parseToken(tokens, position + 1)
        loopBinding.modifiers.push(`&when`)
        break
      case `&while`:
        if (loopBinding.whileNode) {
          throw Error(`Only one &while modifier allowed`)
        }
        ;[position, loopBinding.whileNode] = parseToken(tokens, position + 1)
        loopBinding.modifiers.push(`&while`)
        break
      default:
        throw Error(`Illegal modifier: ${token.value}`)
    }
    token = asNotUndefined(tokens[position])
  }
  return [position, loopBinding]
}

function addToContext(
  bindings: BindingNode[],
  context: Context,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
) {
  for (const binding of bindings) {
    if (context[binding.name]) {
      throw Error(`Variable already defined: ${binding.name}`)
    }
    context[binding.name] = { value: evaluateAstNode(binding.value, contextStack) }
  }
}

function parseLoopBindings(tokens: Token[], position: number, parsers: Parsers): [number, LoopBindingNode[]] {
  let token = asNotUndefined(tokens[position])
  if (!(token.type === `paren` && token.value === `[`)) {
    throw new UnexpectedTokenError(`[`, token)
  }
  position += 1

  const loopBindings: LoopBindingNode[] = []

  token = asNotUndefined(tokens[position])
  while (!(token.type === `paren` && token.value === `]`)) {
    let loopBinding: LoopBindingNode
    ;[position, loopBinding] = parseLoopBinding(tokens, position, parsers)
    loopBindings.push(loopBinding)
    token = asNotUndefined(tokens[position])
  }
  return [position + 1, loopBindings]
}

export const forSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, parsers) => {
    const firstToken = asNotUndefined(tokens[position])
    const { parseToken } = parsers
    let loopBindings: LoopBindingNode[]
    ;[position, loopBindings] = parseLoopBindings(tokens, position, parsers)

    let expression: AstNode
    ;[position, expression] = parseToken(tokens, position)

    const token = asNotUndefined(tokens[position])
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
    const { loopBindings, params } = node
    const expression = asNotUndefined(params[0])

    const result: Arr = []

    const bindingIndices = loopBindings.map(() => 0)
    let abort = false
    while (!abort) {
      const context: Context = {}
      const newContextStack = contextStack.withContext(context)
      let skip = false
      bindingsLoop: for (let bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
        const { binding, letBindings, whenNode, whileNode, modifiers } = asNotUndefined(loopBindings[bindingIndex])
        const coll = asColl(evaluateAstNode(binding.value, newContextStack))
        const seq = isSeq(coll) ? coll : Object.entries(coll)
        if (seq.length === 0) {
          skip = true
          abort = true
          break
        }
        const index = asNotUndefined(bindingIndices[bindingIndex])
        if (index >= seq.length) {
          skip = true
          if (bindingIndex === 0) {
            abort = true
            break
          }
          bindingIndices[bindingIndex] = 0
          bindingIndices[bindingIndex - 1] = asNotUndefined(bindingIndices[bindingIndex - 1]) + 1
          break
        }
        if (context[binding.name]) {
          throw Error(`Variable already defined: ${binding.name}`)
        }
        context[binding.name] = {
          value: asAny(seq[index]),
        }
        for (const modifier of modifiers) {
          switch (modifier) {
            case `&let`:
              addToContext(asNotUndefined(letBindings), context, newContextStack, evaluateAstNode)
              break
            case `&when`:
              if (!evaluateAstNode(asNotUndefined(whenNode), newContextStack)) {
                bindingIndices[bindingIndex] = asNotUndefined(bindingIndices[bindingIndex]) + 1
                skip = true
                break bindingsLoop
              }
              break
            case `&while`:
              if (!evaluateAstNode(asNotUndefined(whileNode), newContextStack)) {
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
