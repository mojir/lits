import type { ReservedSymbolToken } from '../tokenizer/token'
import { asSymbolToken, isA_BinaryOperatorToken, isOperatorToken, isReservedSymbolToken, isSymbolToken } from '../tokenizer/token'
import type { Node, SymbolNode } from '../parser/types'
import { LitsError } from '../errors'
import { NodeTypes } from '../constants/constants'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import type { DefNode } from '../builtin/specialExpressions/def'
import type { SpecialExpressionName } from '../builtin'
import { normalExpressionTypes } from '../builtin/normalExpressions'
import type { IfNode } from '../builtin/specialExpressions/if'
import { isFunctionOperator } from '../tokenizer/operators'
import { isSpecialBuiltinSymbolNode } from '../typeGuards/astNode'
import type { ParserContext } from './ParserContext'
import { parseLet } from './parseLet'
import { parseIfOrUnless } from './parseIfOrUnless'
import { parseCond } from './parseCond'
import { parseSwitch } from './parseSwitch'
import { parseForOrDoseq } from './parseForOrDoseq'
import { parseLoop } from './parseLoop'
import { parseTry } from './parseTry'
import { parseDo } from './parseDo'
import { binaryFunctionalOperatorPrecedence, conditionalOperatorPrecedence, createNamedNormalExpressionNode, exponentiationPrecedence, isAtExpressionEnd, withSourceCodeInfo } from './helpers'
import { parseOperand } from './parseOperand'
import { getPrecedence } from './getPrecedence'
import { fromBinaryOperatorToNode } from './fromBinaryOperatorToNode'
import { parseSymbol } from './parseSymbol'

export function parseExpression(ctx: ParserContext, precedence = 0, moduleScope = false): Node {
  const token = ctx.tryPeek()

  let left: Node

  if (isSymbolToken(token)) {
    switch (token[1]) {
      case 'let':
        return parseLet(ctx, token)
      case 'if':
      case 'unless':
        left = parseIfOrUnless(ctx, token)
        break
      case 'cond':
        left = parseCond(ctx, token)
        break
      case 'switch':
        left = parseSwitch(ctx, token)
        break
      case 'for':
      case 'doseq':
        left = parseForOrDoseq(ctx, token)
        break
      case 'loop':
        left = parseLoop(ctx, token)
        break
      case 'try':
        left = parseTry(ctx, token)
        break
    }
  }
  else if (isReservedSymbolToken(token, 'do')) {
    left = parseDo(ctx)[0]
  }
  else if (isReservedSymbolToken(token, 'export')) {
    if (!moduleScope) {
      throw new LitsError('export is only allowed in module scope', token[2])
    }
    return parseExport(ctx, token)
  }

  left ||= parseOperand(ctx)
  let operator = ctx.tryPeek()

  while (!isAtExpressionEnd(ctx)) {
    if (isA_BinaryOperatorToken(operator)) {
      const name = operator[1]
      const newPrecedece = getPrecedence(name, operator[2])
      if (
        newPrecedece <= precedence
        // ^ (exponentiation) is right associative
        && !(newPrecedece === exponentiationPrecedence && precedence === exponentiationPrecedence)) {
        break
      }
      const symbol: SymbolNode = specialExpressionTypes[name as SpecialExpressionName]
        ? withSourceCodeInfo([NodeTypes.SpecialBuiltinSymbol, specialExpressionTypes[name as SpecialExpressionName]], operator[2])
        : withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes[name]!], operator[2])
      ctx.advance()
      const right = parseExpression(ctx, newPrecedece)
      left = fromBinaryOperatorToNode(operator, symbol, left, right, operator[2])
    }
    else if (isSymbolToken(operator)) {
      if (!isFunctionOperator(operator[1])) {
        break
      }
      const newPrecedence = binaryFunctionalOperatorPrecedence
      if (newPrecedence <= precedence) {
        break
      }
      const operatorSymbol = parseSymbol(ctx)
      const right = parseExpression(ctx, newPrecedence)
      if (isSpecialBuiltinSymbolNode(operatorSymbol)) {
        throw new LitsError('Special expressions are not allowed in binary functional operators', operatorSymbol[2])
      }
      left = createNamedNormalExpressionNode(operatorSymbol, [left, right], operator[2])
    }
    else if (operator?.[1] === '?') {
      if (conditionalOperatorPrecedence <= precedence) {
        break
      }
      ctx.advance()
      const trueNode = parseExpression(ctx)
      if (!isOperatorToken(ctx.tryPeek(), ':')) {
        throw new LitsError('Expected :', ctx.peekSourceCodeInfo())
      }
      ctx.advance()
      const falseNode = parseExpression(ctx)
      left = withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.if, [left, trueNode, falseNode]]], left[2]) satisfies IfNode
    }
    else {
      break
    }

    operator = ctx.tryPeek()
  }

  return left
}

function parseExport(ctx: ParserContext, exportToken: ReservedSymbolToken<'export'>): DefNode {
  ctx.advance()
  const token = ctx.tryPeek()
  if (isSymbolToken(token, 'let')) {
    const letNode = parseLet(ctx, asSymbolToken(token))
    return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_def'], letNode[1][1]]], exportToken[2]) satisfies DefNode
  }
  else {
    throw new LitsError('Expected let', ctx.peekSourceCodeInfo())
  }
}
