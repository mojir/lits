import type { NormalExpressionName } from '../../../reference/api'
import type { SpecialExpressionName } from '../../builtin'
import { normalExpressionTypes } from '../../builtin/normalExpressions'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { NodeTypes } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { AstNode, NormalBuiltinSymbolNode, NormalExpressionNodeExpression, SpecialBuiltinSymbolNode, StringNode } from '../types'
import { isBinaryOperator } from '../../tokenizer/operators'
import type { SourceCodeInfo, StringToken, TokenType } from '../../tokenizer/token'
import { isLBraceToken, isLBracketToken, isLParenToken, isOperatorToken, isRBracketToken, isRParenToken, isSymbolToken } from '../../tokenizer/token'
import { withSourceCodeInfo } from '../helpers'
import type { ParserContext } from '../ParserContext'
import { parseRegexpShorthand } from './parseRegexpShorthand'
import { parseReservedSymbol } from './parseReservedSymbol'
import { parseString } from './parseString'
import { parseSymbol } from './parseSymbol'
import { parseArray } from './parseArray'
import { parseLambdaFunction, parseShorthandLambdaFunction } from './parseFunction'
import { parseFunctionCall } from './parseFunctionCall'
import { parseNumber } from './parseNumber'
import { parseObject } from './parseObject'

export function parseOperand(ctx: ParserContext): AstNode {
  let operand: AstNode = parseOperandPart(ctx)
  let token = ctx.tryPeek()

  while (isOperatorToken(token, '.') || isLBracketToken(token) || isLParenToken(token)) {
    if (token[1] === '.') {
      ctx.advance()
      const symbolToken = ctx.tryPeek()
      if (!isSymbolToken(symbolToken)) {
        throw new LitsError('Expected symbol', ctx.peekSourceCodeInfo())
      }
      const stringNode: StringNode = withSourceCodeInfo([NodeTypes.String, symbolToken[1]], symbolToken[2])
      operand = createAccessorNode(operand, stringNode, token[2])
      ctx.advance()
      token = ctx.tryPeek()
    }
    else if (isLBracketToken(token)) {
      ctx.advance()
      const expression = ctx.parseExpression()
      if (!isRBracketToken(ctx.tryPeek())) {
        throw new LitsError('Expected closing bracket', ctx.peekSourceCodeInfo())
      }
      operand = createAccessorNode(operand, expression, token[2])
      ctx.advance()
      token = ctx.tryPeek()
    }
    else if (isLParenToken(token)) {
      operand = parseFunctionCall(ctx, operand)
      token = ctx.tryPeek()
    }
  }
  return operand
}

function parseOperandPart(ctx: ParserContext): AstNode {
  const token = ctx.peek()

  // Parentheses
  if (isLParenToken(token)) {
    ctx.storePosition()
    const lamdaFunction = parseLambdaFunction(ctx)
    if (lamdaFunction) {
      return lamdaFunction
    }
    ctx.restorePosition()
    ctx.advance()
    const expression = ctx.parseExpression()
    if (!isRParenToken(ctx.peek())) {
      throw new LitsError('Expected closing parenthesis', ctx.peekSourceCodeInfo())
    }
    ctx.advance()
    return expression
  }

  else if (isOperatorToken(token)) {
    const operatorName = token[1]
    if (isBinaryOperator(operatorName)) {
      ctx.advance()
      if (specialExpressionTypes[operatorName as SpecialExpressionName] !== undefined) {
        return withSourceCodeInfo([NodeTypes.SpecialBuiltinSymbol, specialExpressionTypes[operatorName as SpecialExpressionName]], token[2]) satisfies SpecialBuiltinSymbolNode
      }
      return withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes[operatorName as NormalExpressionName] as number], token[2]) satisfies NormalBuiltinSymbolNode
    }

    if (operatorName === '->') {
      return parseShorthandLambdaFunction(ctx)
    }
    else {
      throw new LitsError(`Illegal operator: ${operatorName}`, token[2])
    }
  }

  // Object litteral, e.g. {a: 1, b: 2}
  if (isLBraceToken(token)) {
    return parseObject(ctx)
  }

  // Array litteral, e.g. [1, 2]
  if (isLBracketToken(token)) {
    return parseArray(ctx)
  }

  const tokenType = token[0] as Exclude<
    TokenType,
    | 'Operator' // Handled above
    | 'LParen' // Handled above
    | 'LBrace' // Handled above
    | 'LBracket' // Handled above

    | 'RParen' // Illegal token
    | 'RBrace' // Illegal token
    | 'RBracket' // Illegal token

    | 'MultiLineComment' // Should have been removed
    | 'SingleLineComment' // Should have been removed
    | 'Whitespace' // Should have been removed
  >
  switch (tokenType) {
    case 'Number':
    case 'BasePrefixedNumber':
      return parseNumber(ctx)
    case 'string':
      return parseString(ctx, token as StringToken)
    case 'Symbol': {
      ctx.storePosition()
      const lamdaFunction = parseLambdaFunction(ctx)
      if (lamdaFunction) {
        return lamdaFunction
      }
      ctx.restorePosition()
      return parseSymbol(ctx)
    }
    case 'ReservedSymbol':
      return parseReservedSymbol(ctx)
    case 'RegexpShorthand':
      return parseRegexpShorthand(ctx)

    default:
      throw new LitsError(`Unknown token type: ${tokenType}`, token[2])
  }
}

function createAccessorNode(left: AstNode, right: AstNode, sourceCodeInfo: SourceCodeInfo | undefined): NormalExpressionNodeExpression {
  return withSourceCodeInfo([NodeTypes.NormalExpression, [[NodeTypes.NormalBuiltinSymbol, normalExpressionTypes.get], [left, right]]], sourceCodeInfo)
}
