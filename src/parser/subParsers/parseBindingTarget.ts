import { LitsError } from '../../errors'
import type { AstNode, BindingTarget } from '../types'
import { bindingTargetTypes } from '../types'
import { type Token, assertOperatorToken, isBasePrefixedNumberToken, isLBraceToken, isLBracketToken, isNumberToken, isOperatorToken, isRBraceToken, isRBracketToken, isReservedSymbolToken, isStringToken, isSymbolToken } from '../../tokenizer/token'
import { asUserDefinedSymbolNode, isUserDefinedSymbolNode } from '../../typeGuards/astNode'
import { getSymbolName, withSourceCodeInfo } from '../helpers'
import type { ParserContext } from '../ParserContext'
import { NodeTypes } from '../../constants/constants'
import { parseSymbol } from './parseSymbol'
import { parseString } from './parseString'
import { parseNumber } from './parseNumber'

export interface ParseBindingTargetOptions {
  requireDefaultValue?: true
  noRest?: true
  allowLiteralPatterns?: true
}

export function parseBindingTarget(ctx: ParserContext, { requireDefaultValue, noRest, allowLiteralPatterns }: ParseBindingTargetOptions = {}): BindingTarget {
  const firstToken = ctx.tryPeek()

  // Wildcard _ (only in pattern matching context)
  if (allowLiteralPatterns && isReservedSymbolToken(firstToken, '_')) {
    ctx.advance()
    return withSourceCodeInfo([bindingTargetTypes.wildcard, []], firstToken[2])
  }

  // Literal patterns: number, string, true, false, null (only in pattern matching context)
  if (allowLiteralPatterns && isLiteralToken(firstToken)) {
    if (isNumberToken(firstToken) || isBasePrefixedNumberToken(firstToken)) {
      const node = parseNumber(ctx)
      return withSourceCodeInfo([bindingTargetTypes.literal, [node]], firstToken[2])
    }
    if (isStringToken(firstToken)) {
      const node = parseString(ctx, firstToken)
      return withSourceCodeInfo([bindingTargetTypes.literal, [node]], firstToken[2])
    }
    if (isReservedSymbolToken(firstToken, 'true')) {
      ctx.advance()
      const node: AstNode = withSourceCodeInfo([NodeTypes.ReservedSymbol, 'true'], firstToken[2])
      return withSourceCodeInfo([bindingTargetTypes.literal, [node]], firstToken[2])
    }
    if (isReservedSymbolToken(firstToken, 'false')) {
      ctx.advance()
      const node: AstNode = withSourceCodeInfo([NodeTypes.ReservedSymbol, 'false'], firstToken[2])
      return withSourceCodeInfo([bindingTargetTypes.literal, [node]], firstToken[2])
    }
    if (isReservedSymbolToken(firstToken, 'null')) {
      ctx.advance()
      const node: AstNode = withSourceCodeInfo([NodeTypes.ReservedSymbol, 'null'], firstToken[2])
      return withSourceCodeInfo([bindingTargetTypes.literal, [node]], firstToken[2])
    }
  }

  // Symbol
  if (isSymbolToken(firstToken)) {
    const symbol = parseSymbol(ctx)
    if (!isUserDefinedSymbolNode(symbol)) {
      throw new LitsError('Expected user defined symbol', firstToken[2])
    }

    const defaultValue = parseOptionalDefaulValue(ctx)
    if (requireDefaultValue && !defaultValue) {
      throw new LitsError('Expected assignment', ctx.peekSourceCodeInfo())
    }

    return withSourceCodeInfo([bindingTargetTypes.symbol, [symbol, defaultValue]], firstToken[2])
  }

  // Rest
  if (isOperatorToken(firstToken, '...')) {
    if (noRest) {
      throw new LitsError('Rest element not allowed', firstToken[2])
    }
    ctx.advance()
    const symbol = asUserDefinedSymbolNode(parseSymbol(ctx))
    if (isOperatorToken(ctx.tryPeek(), '=')) {
      throw new LitsError('Rest argument can not have default value', ctx.peekSourceCodeInfo())
    }
    return withSourceCodeInfo([bindingTargetTypes.rest, [symbol[1], undefined]], firstToken[2])
  }

  // Array
  if (isLBracketToken(firstToken)) {
    ctx.advance()
    const elements: (BindingTarget | null)[] = []
    let token = ctx.peek()

    let rest = false
    while (!isRBracketToken(token)) {
      if (rest) {
        throw new LitsError('Rest argument must be last', token[2])
      }
      if (isOperatorToken(token, ',')) {
        elements.push(null)
        ctx.advance()
        token = ctx.peek()
        continue
      }

      const target = parseBindingTarget(ctx, { allowLiteralPatterns })

      if (target[0] === bindingTargetTypes.rest) {
        rest = true
      }

      elements.push(target)
      token = ctx.peek()

      if (!isRBracketToken(token)) {
        assertOperatorToken(token, ',')
        ctx.advance()
      }
      token = ctx.peek()
    }
    ctx.advance()

    const defaultValue = parseOptionalDefaulValue(ctx)
    if (requireDefaultValue && !defaultValue) {
      throw new LitsError('Expected assignment', ctx.peekSourceCodeInfo())
    }

    return withSourceCodeInfo([bindingTargetTypes.array, [elements, defaultValue]], firstToken[2])
  }

  // Object
  if (isLBraceToken(firstToken)) {
    ctx.advance()
    const elements: Record<string, BindingTarget> = {}
    let token = ctx.peek()
    let rest = false
    while (!isRBraceToken(token)) {
      if (rest) {
        throw new LitsError('Rest argument must be last', token[2])
      }
      if (isOperatorToken(token, '...')) {
        rest = true
        ctx.advance()
      }
      // Parse the key symbol - can be any symbol type (including builtins) when using 'as' alias
      const keySymbol = parseSymbol(ctx)
      const keyName = getSymbolName(keySymbol)
      token = ctx.peek()
      if (isReservedSymbolToken(token, 'as')) {
        if (rest) {
          throw new LitsError('Rest argument can not have alias', token[2])
        }
        ctx.advance()
        const name = asUserDefinedSymbolNode(parseSymbol(ctx))
        if (elements[name[1]]) {
          throw new LitsError(`Duplicate binding name: ${name}`, token[2])
        }
        elements[keyName] = withSourceCodeInfo([bindingTargetTypes.symbol, [name, parseOptionalDefaulValue(ctx)]], firstToken[2])
      }
      else if (isRBraceToken(token) || isOperatorToken(token, ',') || isOperatorToken(token, '=')) {
        // Without 'as' alias, the key becomes the binding name - must be user-defined symbol
        const key = asUserDefinedSymbolNode(keySymbol, keySymbol[2])
        if (elements[key[1]]) {
          throw new LitsError(`Duplicate binding name: ${key}`, token[2])
        }
        if (rest && isOperatorToken(ctx.tryPeek(), '=')) {
          throw new LitsError('Rest argument can not have default value', ctx.peekSourceCodeInfo())
        }

        elements[key[1]] = rest
          ? withSourceCodeInfo([bindingTargetTypes.rest, [key[1], parseOptionalDefaulValue(ctx)]], firstToken[2])
          : withSourceCodeInfo([bindingTargetTypes.symbol, [key, parseOptionalDefaulValue(ctx)]], firstToken[2])
      }
      else if (isOperatorToken(token, ':')) {
        ctx.advance()
        token = ctx.peek()
        if (allowLiteralPatterns) {
          // In pattern matching context, allow literals, nested objects/arrays, and variable bindings after ':'
          if (!isLBraceToken(token) && !isLBracketToken(token) && !isLiteralToken(token)) {
            throw new LitsError('Expected literal, object or array pattern', token[2])
          }
        }
        else {
          if (!isLBraceToken(token) && !isLBracketToken(token)) {
            throw new LitsError('Expected object or array', token[2])
          }
        }
        elements[keyName] = parseBindingTarget(ctx, { allowLiteralPatterns })
      }

      if (!isRBraceToken(ctx.peek())) {
        assertOperatorToken(ctx.peek(), ',')
        ctx.advance()
      }
      token = ctx.peek()
    }
    ctx.advance()
    token = ctx.peek()

    const defaultValue = parseOptionalDefaulValue(ctx)
    if (requireDefaultValue && !defaultValue) {
      throw new LitsError('Expected assignment', token[2])
    }

    return withSourceCodeInfo([bindingTargetTypes.object, [elements, defaultValue]], firstToken[2])
  }

  throw new LitsError('Expected symbol', ctx.peekSourceCodeInfo())
}

function parseOptionalDefaulValue(ctx: ParserContext): AstNode | undefined {
  if (isOperatorToken(ctx.tryPeek(), '=')) {
    ctx.advance()
    return ctx.parseExpression()
  }
  return undefined
}

function isLiteralToken(token: Token | undefined): boolean {
  return isNumberToken(token)
    || isBasePrefixedNumberToken(token)
    || isStringToken(token)
    || isReservedSymbolToken(token, 'true')
    || isReservedSymbolToken(token, 'false')
    || isReservedSymbolToken(token, 'null')
}
