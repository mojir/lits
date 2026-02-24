import type { SpecialExpressionName } from '../../builtin'
import { normalExpressionTypes } from '../../builtin/normalExpressions'
import type { IfNode } from '../../builtin/specialExpressions/if'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { NodeTypes } from '../../constants/constants'
import { LitsError } from '../../errors'
import { isFunctionOperator } from '../../tokenizer/operators'
import { isA_BinaryOperatorToken, isOperatorToken, isReservedSymbolToken, isSymbolToken } from '../../tokenizer/token'
import type { TokenStream } from '../../tokenizer/tokenize'
import { isSpecialBuiltinSymbolNode } from '../../typeGuards/astNode'
import { binaryFunctionalOperatorPrecedence, conditionalOperatorPrecedence, createNamedNormalExpressionNode, exponentiationPrecedence, fromBinaryOperatorToNode, isAtExpressionEnd, withSourceCodeInfo } from '../helpers'
import { ParserContext } from '../ParserContext'
import type { AstNode, SymbolNode } from '../types'
import { getPrecedence } from '../getPrecedence'
import { parseCond } from './parseCond'
import { parseDo } from './parseDo'
import { parseForOrDoseq } from './parseForOrDoseq'
import { parseIfOrUnless } from './parseIfOrUnless'
import { parseLet } from './parseLet'
import { parseLoop } from './parseLoop'
import { parseOperand } from './parseOperand'
import { parseSwitch } from './parseSwitch'
import { parseSymbol } from './parseSymbol'
import { parseTry } from './parseTry'

export function createParserContext(tokenStream: TokenStream): ParserContext {
  const ctx = new ParserContext(tokenStream)
  ctx.parseExpression = (precedence = 0) => parseExpression(ctx, precedence)
  return ctx
}

export function parseExpression(ctx: ParserContext, precedence = 0): AstNode {
  const token = ctx.tryPeek()

  let left: AstNode

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
