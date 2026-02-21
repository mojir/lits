import type { LambdaNode } from '../../builtin/specialExpressions/functions'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { NodeTypes } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { AstNode, BindingTarget } from '../types'
import { bindingTargetTypes } from '../types'
import { assertLParenToken, isLParenToken, isOperatorToken, isRParenToken, isReservedSymbolToken, isSymbolToken } from '../../tokenizer/token'
import { withSourceCodeInfo } from '../helpers'
import type { ParserContext } from '../ParserContext'
import { parseBindingTarget } from './parseBindingTarget'
import { parseDo } from './parseDo'
import { parseSymbol } from './parseSymbol'

const placeholderRegexp = /^\$([1-9]\d?)?$/
const maxShorthandLambdaArity = 20

export function parseLambdaFunction(ctx: ParserContext): LambdaNode | null {
  const firstToken = ctx.peek()

  if (isLParenToken(firstToken)
    && isSymbolToken(ctx.peekAhead(1))
    && isOperatorToken(ctx.peekAhead(2), '->')) {
    return null
  }

  try {
    const functionArguments = parseFunctionArguments(ctx)

    if (!isOperatorToken(ctx.peek(), '->')) {
      return null
    }
    ctx.advance()
    let nodes: AstNode[] | undefined
    let docString = ''
    if (isReservedSymbolToken(ctx.peek(), 'do')) {
      const parsedDo = parseDo(ctx, true)
      docString = parsedDo[1]
      nodes = parsedDo[0][1][1]
    }
    else {
      nodes = [ctx.parseExpression()]
    }

    return withSourceCodeInfo([
      NodeTypes.SpecialExpression,
      [
        specialExpressionTypes['0_lambda'],
        [
          functionArguments,
          nodes,
        ],
        docString,
      ],
    ], firstToken[2]) satisfies LambdaNode
  }
  catch {
    return null
  }
}

function parseFunctionArguments(ctx: ParserContext): BindingTarget[] {
  const firstToken = ctx.peek()
  if (isSymbolToken(firstToken)) {
    return [withSourceCodeInfo([bindingTargetTypes.symbol, [parseSymbol(ctx), undefined]], firstToken[2])]
  }

  assertLParenToken(firstToken)
  ctx.advance()

  let rest = false
  let defaults = false
  const functionArguments: BindingTarget[] = []
  while (!ctx.isAtEnd() && !isRParenToken(ctx.peek()) && !isSymbolToken(ctx.peek(), 'let')) {
    if (rest) {
      throw new LitsError('Rest argument must be last', ctx.peekSourceCodeInfo())
    }
    const bindingTarget = parseBindingTarget(ctx)
    if (bindingTarget[1][1] !== undefined) {
      defaults = true
    }
    if (bindingTarget[0] === bindingTargetTypes.rest) {
      rest = true
    }
    if (defaults && !bindingTarget[1][1]) {
      throw new LitsError('Default arguments must be last', ctx.peekSourceCodeInfo())
    }
    functionArguments.push(bindingTarget)

    if (!isOperatorToken(ctx.peek(), ',') && !isRParenToken(ctx.peek()) && !isSymbolToken(ctx.peek(), 'let')) {
      throw new LitsError('Expected comma or closing parenthesis', ctx.peekSourceCodeInfo())
    }
    if (isOperatorToken(ctx.peek(), ',')) {
      ctx.advance()
    }
  }

  if (!isRParenToken(ctx.peek())) {
    throw new LitsError('Expected closing parenthesis', ctx.peekSourceCodeInfo())
  }

  ctx.advance()

  return functionArguments
}
export function parseShorthandLambdaFunction(ctx: ParserContext): LambdaNode {
  const firstToken = ctx.peek()
  ctx.advance()
  // TODO, do not like this...
  const startPos = ctx.getPosition()

  let nodes: AstNode[] | undefined
  let docString = ''
  if (isReservedSymbolToken(ctx.peek(), 'do')) {
    const parsedDo = parseDo(ctx, true)
    docString = parsedDo[1]
    nodes = parsedDo[0][1][1]
  }
  else {
    nodes = [ctx.parseExpression()]
  }

  const endPos = ctx.getPosition() - 1

  let arity = 0
  let dollar1: 'NOT_SET' | 'WITH_1' | 'NAKED' = 'NOT_SET' // referring to argument bindings. $ = NAKED, $1, $2, $3, etc = WITH_1
  for (let pos = startPos; pos <= endPos; pos += 1) {
    const token = ctx.getTokenAt(pos)!
    if (isSymbolToken(token)) {
      const match = placeholderRegexp.exec(token[1])
      if (match) {
        const number = match[1] ?? '1'
        if (number === '1') {
          const mixedPercent1 = (!match[1] && dollar1 === 'WITH_1') || (match[1] && dollar1 === 'NAKED')
          if (mixedPercent1)
            throw new LitsError('Please make up your mind, either use $ or $1', firstToken[2])

          dollar1 = match[1] ? 'WITH_1' : 'NAKED'
        }

        arity = Math.max(arity, Number(number))
        if (arity > maxShorthandLambdaArity)
          throw new LitsError('Can\'t specify more than 20 arguments', firstToken[2])
      }
    }
  }

  const functionArguments: BindingTarget[] = []

  for (let i = 1; i <= arity; i += 1) {
    if (i === 1 && dollar1 === 'NAKED') {
      functionArguments.push(withSourceCodeInfo([bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, '$'], undefined]], firstToken[2]))
    }
    else {
      functionArguments.push(withSourceCodeInfo([bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, `$${i}`], undefined]], firstToken[2]))
    }
  }

  const node: LambdaNode = withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_lambda'], [
    functionArguments,
    nodes,
  ], docString]], firstToken[2])

  return node
}
