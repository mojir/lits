import type { SpecialExpressionNode } from '..'
import type { FindUnresolvedIdentifiers, UnresolvedIdentifier, UnresolvedIdentifiers } from '../../analyze'
import { AstNodeType } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { ContextStack } from '../../evaluator/ContextStack'
import type { Context, EvaluateAstNode } from '../../evaluator/interface'
import type { Any, Arr } from '../../interface'
import type { AstNode, BindingNode, CommonSpecialExpressionNode, ParseState } from '../../parser/interface'
import { assertLBracketToken, assertRParenToken, isRBracketToken } from '../../tokenizer/common/commonTokens'
import type { SourceCodeInfo, TokenStream } from '../../tokenizer/interface'
import { isP_ModifierToken } from '../../tokenizer/polish/polishTokens'
import { asToken } from '../../tokenizer/tokens'
import { getTokenDebugData } from '../../tokenizer/utils'
import { asNonUndefined } from '../../typeGuards'
import { asAstNode } from '../../typeGuards/astNode'
import { asAny, asColl, isSeq } from '../../typeGuards/lits'
import type { Builtin, BuiltinSpecialExpression, ParserHelpers } from '../interface'

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

function parseLoopBinding(
  tokenStream: TokenStream,
  parseState: ParseState,
  { parseBinding, parseBindings, parseToken }: ParserHelpers,
): LoopBindingNode {
  const bindingNode = parseBinding(tokenStream, parseState)

  const loopBinding: LoopBindingNode = {
    b: bindingNode,
    m: [],
  }

  let tkn = asToken(tokenStream.tokens[parseState.position])
  while (isP_ModifierToken(tkn)) {
    const modifier = tkn[1]
    switch (modifier) {
      case '&let':
        if (loopBinding.l) {
          throw new LitsError('Only one &let modifier allowed', getTokenDebugData(tkn)?.sourceCodeInfo)
        }
        parseState.position += 1
        loopBinding.l = parseBindings(tokenStream, parseState)
        loopBinding.m.push('&let')
        break
      case '&when':
        if (loopBinding.wn) {
          throw new LitsError('Only one &when modifier allowed', getTokenDebugData(tkn)?.sourceCodeInfo)
        }
        parseState.position += 1
        loopBinding.wn = parseToken(tokenStream, parseState)
        loopBinding.m.push('&when')
        break
      case '&while':
        if (loopBinding.we) {
          throw new LitsError('Only one &while modifier allowed', getTokenDebugData(tkn)?.sourceCodeInfo)
        }
        parseState.position += 1
        loopBinding.we = parseToken(tokenStream, parseState)
        loopBinding.m.push('&while')
        break
      case '&rest':
        throw new LitsError(`Illegal modifier: ${modifier}`, getTokenDebugData(tkn)?.sourceCodeInfo)
      default:
        throw new LitsError(`Illegal modifier: ${modifier satisfies never}`, getTokenDebugData(tkn)?.sourceCodeInfo)
    }
    tkn = asToken(tokenStream.tokens[parseState.position])
  }
  return loopBinding
}

function addToContext(
  bindings: BindingNode[],
  context: Context,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
  sourceCodeInfo?: SourceCodeInfo,
) {
  for (const binding of bindings) {
    if (context[binding.n])
      throw new LitsError(`Variable already defined: ${binding.n}.`, sourceCodeInfo)

    context[binding.n] = { value: evaluateAstNode(binding.v, contextStack) }
  }
}

function parseLoopBindings(
  tokenStream: TokenStream,
  parseState: ParseState,
  parsers: ParserHelpers,
): LoopBindingNode[] {
  assertLBracketToken(tokenStream.tokens[parseState.position++])

  const loopBindings: LoopBindingNode[] = []

  let tkn = asToken(tokenStream.tokens[parseState.position])
  while (!isRBracketToken(tkn)) {
    loopBindings.push(parseLoopBinding(tokenStream, parseState, parsers))
    tkn = asToken(tokenStream.tokens[parseState.position])
  }
  parseState.position += 1
  return loopBindings
}
function evaluateLoop(
  returnResult: boolean,
  node: SpecialExpressionNode,
  contextStack: ContextStack,
  evaluateAstNode: EvaluateAstNode,
) {
  const sourceCodeInfo = getTokenDebugData(node.token)?.sourceCodeInfo
  const { l: loopBindings, p: params } = node as LoopNode

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
      const coll = asColl(evaluateAstNode(binding.v, newContextStack), sourceCodeInfo)
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
      if (context[binding.n])
        throw new LitsError(`Variable already defined: ${binding.n}.`, sourceCodeInfo)

      context[binding.n] = {
        value: asAny(seq[index], sourceCodeInfo),
      }
      for (const modifier of modifiers) {
        switch (modifier) {
          case '&let':
            addToContext(
              asNonUndefined(letBindings, sourceCodeInfo),
              context,
              newContextStack,
              evaluateAstNode,
              sourceCodeInfo,
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
  findUnresolvedIdentifiers: FindUnresolvedIdentifiers,
  builtin: Builtin,
): UnresolvedIdentifiers {
  const result = new Set<UnresolvedIdentifier>()
  const newContext: Context = {}
  const { l: loopBindings } = node
  loopBindings.forEach((loopBinding) => {
    const { b: binding, l: letBindings, wn: whenNode, we: whileNode } = loopBinding
    findUnresolvedIdentifiers([binding.v], contextStack.create(newContext), builtin).forEach(symbol =>
      result.add(symbol),
    )
    newContext[binding.n] = { value: true }
    if (letBindings) {
      letBindings.forEach((letBinding) => {
        findUnresolvedIdentifiers([letBinding.v], contextStack.create(newContext), builtin).forEach(symbol =>
          result.add(symbol),
        )
        newContext[letBinding.n] = { value: true }
      })
    }
    if (whenNode) {
      findUnresolvedIdentifiers([whenNode], contextStack.create(newContext), builtin).forEach(symbol =>
        result.add(symbol),
      )
    }
    if (whileNode) {
      findUnresolvedIdentifiers([whileNode], contextStack.create(newContext), builtin).forEach(symbol =>
        result.add(symbol),
      )
    }
  })
  findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin).forEach(symbol =>
    result.add(symbol),
  )
  return result
}

export const forSpecialExpression: BuiltinSpecialExpression<Any, ForNode> = {
  polishParse: (tokenStream, parseState, firstToken, parsers) => {
    const { parseTokensUntilClosingBracket } = parsers
    const loopBindings = parseLoopBindings(tokenStream, parseState, parsers)

    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: ForNode = {
      n: 'for',
      t: AstNodeType.SpecialExpression,
      l: loopBindings,
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  },
  paramCount: 1,
  evaluate: (node, contextStack, helpers) => evaluateLoop(true, node, contextStack, helpers.evaluateAstNode),
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => analyze(node, contextStack, findUnresolvedIdentifiers, builtin),
}

export const doseqSpecialExpression: BuiltinSpecialExpression<null, DoSeqNode> = {
  polishParse: (tokenStream, parseState, firstToken, parsers) => {
    const { parseTokensUntilClosingBracket } = parsers
    const loopBindings = parseLoopBindings(tokenStream, parseState, parsers)

    const params = parseTokensUntilClosingBracket(tokenStream, parseState)
    assertRParenToken(tokenStream.tokens[parseState.position++])

    const node: DoSeqNode = {
      n: 'doseq',
      t: AstNodeType.SpecialExpression,
      l: loopBindings,
      p: params,
      token: getTokenDebugData(firstToken) && firstToken,
    }

    return node
  },
  paramCount: 1,
  evaluate: (node, contextStack, helpers) => {
    evaluateLoop(false, node, contextStack, helpers.evaluateAstNode)
    return null
  },
  findUnresolvedIdentifiers: (node, contextStack, { findUnresolvedIdentifiers, builtin }) => analyze(node, contextStack, findUnresolvedIdentifiers, builtin),
}
