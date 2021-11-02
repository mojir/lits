import { LitsError } from '../../errors'
import { Context, ContextStack, EvaluateAstNode } from '../../evaluator/interface'
import { Any, Arr } from '../../interface'
import { AstNode, BindingNode, SpecialExpressionNode } from '../../parser/interface'
import { Token, SourceCodeInfo } from '../../tokenizer/interface'
import { any, astNode, asValue, collection, sequence, token } from '../../utils/assertion'
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

  let tkn = token.as(tokens[position], `EOF`)
  while (tkn.type === `modifier`) {
    switch (tkn.value) {
      case `&let`:
        if (loopBinding.letBindings) {
          throw new LitsError(`Only one &let modifier allowed`, tkn.sourceCodeInfo)
        }
        ;[position, loopBinding.letBindings] = parseBindings(tokens, position + 1)
        loopBinding.modifiers.push(`&let`)
        break
      case `&when`:
        if (loopBinding.whenNode) {
          throw new LitsError(`Only one &when modifier allowed`, tkn.sourceCodeInfo)
        }
        ;[position, loopBinding.whenNode] = parseToken(tokens, position + 1)
        loopBinding.modifiers.push(`&when`)
        break
      case `&while`:
        if (loopBinding.whileNode) {
          throw new LitsError(`Only one &while modifier allowed`, tkn.sourceCodeInfo)
        }
        ;[position, loopBinding.whileNode] = parseToken(tokens, position + 1)
        loopBinding.modifiers.push(`&while`)
        break
      default:
        throw new LitsError(`Illegal modifier: ${tkn.value}`, tkn.sourceCodeInfo)
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
  sourceCodeInfo: SourceCodeInfo,
) {
  for (const binding of bindings) {
    if (context[binding.name]) {
      throw new LitsError(`Variable already defined: ${binding.name}`, sourceCodeInfo)
    }
    context[binding.name] = { value: evaluateAstNode(binding.value, contextStack) }
  }
}

function parseLoopBindings(tokens: Token[], position: number, parsers: Parsers): [number, LoopBindingNode[]] {
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

export const forSpecialExpression: BuiltinSpecialExpression<Any> = {
  parse: (tokens, position, parsers) => {
    const firstToken = token.as(tokens[position], `EOF`)
    const { parseToken } = parsers
    let loopBindings: LoopBindingNode[]
    ;[position, loopBindings] = parseLoopBindings(tokens, position, parsers)

    let expression: AstNode
    ;[position, expression] = parseToken(tokens, position)

    token.assert(tokens[position], `EOF`, { type: `paren`, value: `)` })

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
    const sourceCodeInfo = node.token.sourceCodeInfo
    const { loopBindings, params } = node
    const expression = astNode.as(params[0], sourceCodeInfo)

    const result: Arr = []

    const bindingIndices = loopBindings.map(() => 0)
    let abort = false
    while (!abort) {
      const context: Context = {}
      const newContextStack = contextStack.withContext(context)
      let skip = false
      bindingsLoop: for (let bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
        const { binding, letBindings, whenNode, whileNode, modifiers } = asValue(
          loopBindings[bindingIndex],
          sourceCodeInfo,
        )
        const coll = collection.as(evaluateAstNode(binding.value, newContextStack), sourceCodeInfo)
        const seq = sequence.is(coll) ? coll : Object.entries(coll)
        if (seq.length === 0) {
          skip = true
          abort = true
          break
        }
        const index = asValue(bindingIndices[bindingIndex], sourceCodeInfo)
        if (index >= seq.length) {
          skip = true
          if (bindingIndex === 0) {
            abort = true
            break
          }
          bindingIndices[bindingIndex] = 0
          bindingIndices[bindingIndex - 1] = asValue(bindingIndices[bindingIndex - 1], sourceCodeInfo) + 1
          break
        }
        if (context[binding.name]) {
          throw new LitsError(`Variable already defined: ${binding.name}`, sourceCodeInfo)
        }
        context[binding.name] = {
          value: any.as(seq[index], sourceCodeInfo),
        }
        for (const modifier of modifiers) {
          switch (modifier) {
            case `&let`:
              addToContext(
                asValue(letBindings, sourceCodeInfo),
                context,
                newContextStack,
                evaluateAstNode,
                sourceCodeInfo,
              )
              break
            case `&when`:
              if (!evaluateAstNode(astNode.as(whenNode, sourceCodeInfo), newContextStack)) {
                bindingIndices[bindingIndex] = asValue(bindingIndices[bindingIndex], sourceCodeInfo) + 1
                skip = true
                break bindingsLoop
              }
              break
            case `&while`:
              if (!evaluateAstNode(astNode.as(whileNode, sourceCodeInfo), newContextStack)) {
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
