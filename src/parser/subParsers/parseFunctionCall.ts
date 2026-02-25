import type { SpecialExpression, SpecialExpressionType } from '../../builtin'
import { builtin } from '../../builtin'
import type { AndNode } from '../../builtin/specialExpressions/and'
import type { ArrayNode } from '../../builtin/specialExpressions/array'
import type { DefinedNode } from '../../builtin/specialExpressions/defined'
import type { ImportNode } from '../../builtin/specialExpressions/import'
import type { ObjectNode } from '../../builtin/specialExpressions/object'
import type { OrNode } from '../../builtin/specialExpressions/or'
import type { QqNode } from '../../builtin/specialExpressions/qq'
import type { RecurNode } from '../../builtin/specialExpressions/recur'
import type { ThrowNode } from '../../builtin/specialExpressions/throw'
import { specialExpressionTypes } from '../../builtin/specialExpressionTypes'
import { NodeTypes } from '../../constants/constants'
import { LitsError } from '../../errors'
import type { AstNode, NormalExpressionNodeExpression, SymbolNode } from '../types'
import { isOperatorToken, isRParenToken } from '../../tokenizer/token'
import { isNormalBuiltinSymbolNode, isSpecialBuiltinSymbolNode, isUserDefinedSymbolNode } from '../../typeGuards/astNode'
import { assertNumberOfParams } from '../../utils/arity'
import { createNamedNormalExpressionNode, withSourceCodeInfo } from '../helpers'
import type { ParserContext } from '../ParserContext'

export function parseFunctionCall(ctx: ParserContext, symbol: AstNode): AstNode {
  ctx.advance()

  const params: AstNode[] = []
  while (!ctx.isAtEnd() && !isRParenToken(ctx.tryPeek())) {
    if (isOperatorToken(ctx.tryPeek(), '...')) {
      ctx.advance()
      params.push(withSourceCodeInfo([NodeTypes.Spread, ctx.parseExpression()], ctx.peekSourceCodeInfo()))
    }
    else {
      params.push(ctx.parseExpression())
    }
    const nextToken = ctx.tryPeek()
    if (!isOperatorToken(nextToken, ',') && !isRParenToken(nextToken)) {
      throw new LitsError('Expected comma or closing parenthesis', ctx.tryPeek()?.[2])
    }
    if (isOperatorToken(nextToken, ',')) {
      ctx.advance()
    }
  }
  if (!isRParenToken(ctx.tryPeek())) {
    throw new LitsError('Expected closing parenthesis', ctx.peekSourceCodeInfo())
  }
  ctx.advance()

  if (isSpecialBuiltinSymbolNode(symbol)) { // Named function
    const specialExpressionType = symbol[1]

    // Handle import specially — extract module name as a string from the symbol argument
    if (specialExpressionType === specialExpressionTypes.import) {
      if (params.length !== 1) {
        throw new LitsError(`import expects exactly 1 argument, got ${params.length}`, symbol[2])
      }
      const param = params[0]!
      if (!isUserDefinedSymbolNode(param)) {
        throw new LitsError('import expects a module name (symbol), got a non-symbol argument', param[2] ?? symbol[2])
      }
      const moduleName = param[1]
      return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionType, moduleName]], symbol[2]) satisfies ImportNode
    }

    const type = specialExpressionType as Exclude<
      SpecialExpressionType,
      | typeof specialExpressionTypes.for
      | typeof specialExpressionTypes.if
      | typeof specialExpressionTypes.unless
      | typeof specialExpressionTypes.cond
      | typeof specialExpressionTypes.match
      | typeof specialExpressionTypes.let
      | typeof specialExpressionTypes.block
      | typeof specialExpressionTypes.loop
      | typeof specialExpressionTypes.try
      | typeof specialExpressionTypes.doseq
      | typeof specialExpressionTypes.import
    >
    const specialExpression: SpecialExpression = builtin.specialExpressions[type]
    assertNumberOfParams(specialExpression.arity, params.length, symbol[2])
    switch (type) {
      case specialExpressionTypes['||']:
        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies OrNode
      case specialExpressionTypes['&&']:
        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies AndNode
      case specialExpressionTypes.recur:
        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies RecurNode
      case specialExpressionTypes.array:
        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies ArrayNode
      case specialExpressionTypes.object:
        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies ObjectNode
      case specialExpressionTypes['??']:
        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]) satisfies QqNode
      case specialExpressionTypes['defined?']: {
        const [param] = params
        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, param as SymbolNode]], symbol[2]) satisfies DefinedNode
      }
      case specialExpressionTypes.throw: {
        const [param] = params
        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, param!]], symbol[2]) satisfies ThrowNode
      }
      case specialExpressionTypes['0_lambda']:
        throw new LitsError(`${type} is not allowed`, symbol[2])
      /* v8 ignore next 2 */
      default:
        throw new LitsError(`Unknown special expression: ${type satisfies never}`, symbol[2])
    }
  }
  else if (isNormalBuiltinSymbolNode(symbol) || isUserDefinedSymbolNode(symbol)) {
    return createNamedNormalExpressionNode(symbol, params, symbol[2])
  }

  else {
    return withSourceCodeInfo([NodeTypes.NormalExpression, [symbol, params]], symbol[2]) satisfies NormalExpressionNodeExpression
  }
}
