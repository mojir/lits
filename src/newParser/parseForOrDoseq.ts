import { getAllBindingTargetNames } from '../builtin/bindingNode'
import type { DoSeqNode, ForNode, LoopBindingNode } from '../builtin/specialExpressions/loops'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { NodeTypes } from '../constants/constants'
import { LitsError } from '../errors'
import type { BindingNode, Node } from '../parser/types'
import { bindingTargetTypes } from '../parser/types'
import type { SymbolToken, Token } from '../tokenizer/token'
import { asSymbolToken, assertLParenToken, assertOperatorToken, assertRParenToken, assertReservedSymbolToken, isOperatorToken, isRParenToken, isReservedSymbolToken, isSymbolToken } from '../tokenizer/token'
import { asUserDefinedSymbolNode } from '../typeGuards/astNode'
import { withSourceCodeInfo } from './helpers'
import { parseLet } from './parseLet'
import type { ParserContext } from './ParserContext'
import { parseSymbol } from './parseSymbol'

type InternalLoopBindingDelimiter = 'let' | 'when' | 'while'

export function parseForOrDoseq(ctx: ParserContext, firstToken: SymbolToken): ForNode | DoSeqNode {
  const isDoseq = firstToken[1] === 'doseq'
  ctx.advance()

  assertLParenToken(ctx.tryPeek())
  ctx.advance()

  const forLoopBindings: LoopBindingNode[] = []

  while (!ctx.isAtEnd() && !isRParenToken(ctx.tryPeek())) {
    const loopBinding = parseForLoopBinding(ctx)
    const existingBoundNames = forLoopBindings.flatMap(b => Object.keys(getAllBindingTargetNames(b[0][1][0])))
    const newBoundNames = getAllBindingTargetNames(loopBinding[0][1][0])
    if (Object.keys(newBoundNames).some(n => existingBoundNames.includes(n))) {
      throw new LitsError('Duplicate binding', loopBinding[0][2])
    }
    forLoopBindings.push(loopBinding)
    if (isOperatorToken(ctx.tryPeek(), ',')) {
      ctx.advance()
    }
  }

  assertRParenToken(ctx.tryPeek())
  ctx.advance()

  assertOperatorToken(ctx.tryPeek(), '->')
  ctx.advance()

  const expression = ctx.parseExpression()

  return isDoseq
    ? withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.doseq, forLoopBindings, expression]], firstToken[2]) satisfies DoSeqNode
    : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.for, forLoopBindings, expression]], firstToken[2]) satisfies ForNode
}

function parseForLoopBinding(ctx: ParserContext): LoopBindingNode {
  const bindingNode = parseBinding(ctx)

  const modifiers: Array<'&let' | '&when' | '&while'> = []
  let token = ctx.peek()

  assertInternalLoopBindingDelimiter(token, ['let', 'when', 'while'])

  const letBindings: BindingNode[] = []
  if (token[1] === 'let') {
    modifiers.push('&let')
    while (isSymbolToken(token, 'let')) {
      const letNode = parseLet(ctx, token)
      const existingBoundNames = letBindings.flatMap(b => Object.keys(getAllBindingTargetNames(b[1][0])))
      const newBoundNames = Object.keys(getAllBindingTargetNames(letNode[1][1][1][0]))
      if (newBoundNames.some(n => existingBoundNames.includes(n))) {
        throw new LitsError('Duplicate binding', letNode[1][1][2])
      }

      letBindings.push(letNode[1][1])
      token = ctx.peek()
      assertInternalLoopBindingDelimiter(token, ['let', 'when', 'while'])
      token = ctx.peek()
    }
  }

  let whenNode: Node | undefined
  let whileNode: Node | undefined
  while (
    isReservedSymbolToken(token, 'when')
    || isReservedSymbolToken(token, 'while')
  ) {
    ctx.advance()

    if (token[1] === 'when') {
      modifiers.push('&when')
      whenNode = ctx.parseExpression()
    }
    else {
      modifiers.push('&while')
      whileNode = ctx.parseExpression()
    }
    token = ctx.peek()

    const symbols: ('when' | 'while')[] = modifiers.includes('&when') && modifiers.includes('&while')
      ? []
      : modifiers.includes('&when')
        ? ['while']
        : ['when']

    assertInternalLoopBindingDelimiter(token, symbols)
    token = ctx.peek()
  }

  assertInternalLoopBindingDelimiter(token, [])

  return [bindingNode, letBindings, whenNode, whileNode] satisfies LoopBindingNode
}

function parseBinding(ctx: ParserContext): BindingNode {
  const firstToken = asSymbolToken(ctx.tryPeek())
  const name = asUserDefinedSymbolNode(parseSymbol(ctx))

  assertReservedSymbolToken(ctx.tryPeek(), 'in')
  ctx.advance()

  const value = ctx.parseExpression()

  const node: BindingNode = withSourceCodeInfo(
    [
      NodeTypes.Binding,
      [
        withSourceCodeInfo([bindingTargetTypes.symbol, [name, undefined]], firstToken[2]),
        value,
      ],
    ],
    firstToken[2],
  )
  return node
}

function assertInternalLoopBindingDelimiter(token: Token, symbols: InternalLoopBindingDelimiter[]): void {
  if (!isInternalLoopBindingDelimiter(token, symbols)) {
    const symbolsString = `${[...symbols, ','].map(symbol => `"${symbol}"`).join(', ')} or ")"`
    throw new LitsError(`Expected symbol ${symbolsString}`, token[2])
  }
}

function isInternalLoopBindingDelimiter(token: Token, symbols: InternalLoopBindingDelimiter[]): boolean {
  // end of loop binding
  if (isOperatorToken(token, ',') || isRParenToken(token)) {
    return true
  }
  for (const symbol of symbols) {
    if (symbol === 'let' && isSymbolToken(token, 'let')) {
      return true
    }
    if (['when', 'while'].includes(symbol) && isReservedSymbolToken(token, symbol as 'when' | 'while')) {
      return true
    }
  }
  return false
}
