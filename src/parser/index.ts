import type { TokenStream } from '../tokenizer/tokenize'
import { LitsError } from '../errors'
import { asSymbolToken, isA_BinaryOperatorToken, isOperatorToken, isReservedSymbolToken, isSymbolToken } from '../tokenizer/token'
import type { ReservedSymbolToken } from '../tokenizer/token'
import { NodeTypes } from '../constants/constants'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import type { DefNode } from '../builtin/specialExpressions/def'
import type { SpecialExpressionName } from '../builtin'
import { normalExpressionTypes } from '../builtin/normalExpressions'
import type { IfNode } from '../builtin/specialExpressions/if'
import { isFunctionOperator } from '../tokenizer/operators'
import { isSpecialBuiltinSymbolNode } from '../typeGuards/astNode'
import { ParserContext } from './ParserContext'
import type { AstNode, SymbolNode } from './types'
import { parseLet } from './subParsers/parseLet'
import { parseIfOrUnless } from './subParsers/parseIfOrUnless'
import { parseCond } from './subParsers/parseCond'
import { parseSwitch } from './subParsers/parseSwitch'
import { parseForOrDoseq } from './subParsers/parseForOrDoseq'
import { parseLoop } from './subParsers/parseLoop'
import { parseTry } from './subParsers/parseTry'
import { parseDo } from './subParsers/parseDo'
import { binaryFunctionalOperatorPrecedence, conditionalOperatorPrecedence, createNamedNormalExpressionNode, exponentiationPrecedence, fromBinaryOperatorToNode, isAtExpressionEnd, withSourceCodeInfo } from './helpers'
import { parseOperand } from './subParsers/parseOperand'
import { getPrecedence } from './subParsers/getPrecedence'
import { parseSymbol } from './subParsers/parseSymbol'

export function parse(tokenStream: TokenStream): AstNode[] {
  tokenStream.tokens.forEach((token) => {
    if (token[0] === 'Error') {
      throw new LitsError(token[3], token[2])
    }
  })

  const nodes: AstNode[] = []

  const ctx = new ParserContext(tokenStream)
  ctx.parseExpression = (precedence = 0, moduleScope = false) => parseExpression(ctx, precedence, moduleScope)

  while (!ctx.isAtEnd()) {
    nodes.push(parseExpression(ctx, 0, true))
    if (isOperatorToken(ctx.tryPeek(), ';')) {
      ctx.advance()
    }
    else {
      if (!ctx.isAtEnd()) {
        throw new LitsError('Expected ;', ctx.peekSourceCodeInfo())
      }
    }
  }

  return nodes
}

function parseExpression(ctx: ParserContext, precedence = 0, moduleScope = false): AstNode {
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
