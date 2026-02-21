import type { NormalExpressionName } from '../../reference/api'
import type { SpecialExpressionName } from '../builtin'
import { allNormalExpressions, normalExpressionTypes } from '../builtin/normalExpressions'
import type { AndNode } from '../builtin/specialExpressions/and'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { NodeTypes } from '../constants/constants'
import { LitsError } from '../errors'
import type { OperatorToken, SourceCodeInfo } from '../tokenizer/token'
import { isOperatorToken, isReservedSymbolToken } from '../tokenizer/token'
import { isNormalBuiltinSymbolNode, isUserDefinedSymbolNode } from '../typeGuards/astNode'
import { assertNumberOfParams } from '../utils/arity'
import type { AstNode, BindingTarget, NormalBuiltinSymbolNode, NormalExpressionNodeWithName, SymbolNode, UserDefinedSymbolNode } from './types'
import type { ParserContext } from './ParserContext'

export const exponentiationPrecedence = 12
export const binaryFunctionalOperatorPrecedence = 3
export const conditionalOperatorPrecedence = 1

export function withSourceCodeInfo<T extends AstNode | BindingTarget>(node: T, sourceCodeInfo: SourceCodeInfo | undefined): T {
  if (sourceCodeInfo) {
    node[2] = sourceCodeInfo
  }
  return node
}

export function stringToSymbolNode(value: string, sourceCodeInfo: SourceCodeInfo | undefined): SymbolNode {
  if (specialExpressionTypes[value as SpecialExpressionName] !== undefined && value !== 'fn' && value !== 'def' && value !== 'defn') {
    return withSourceCodeInfo([NodeTypes.SpecialBuiltinSymbol, specialExpressionTypes[value as SpecialExpressionName]], sourceCodeInfo) satisfies SymbolNode
  }
  if (normalExpressionTypes[value as NormalExpressionName] !== undefined) {
    return withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes[value as NormalExpressionName] as number], sourceCodeInfo) satisfies SymbolNode
  }
  return withSourceCodeInfo([NodeTypes.UserDefinedSymbol, value], sourceCodeInfo) satisfies SymbolNode
}

export function stringFromQuotedSymbol(value: string): string {
  return value.substring(1, value.length - 1)
    .replace(
      /(\\{2})|(\\')|\\(.)/g,
      (
        _,
        backslash: string,
        singleQuote: string,
        normalChar: string,
      ) => {
        if (backslash) {
          return '\\'
        }
        if (singleQuote) {
          return '\''
        }
        return `\\${normalChar}`
      },
    )
}

// Reverse lookup tables for getting symbol names from builtin types
const normalExpressionNames: string[] = Object.entries(normalExpressionTypes).reduce((acc, [name, index]) => {
  acc[index] = name
  return acc
}, [] as string[])

const specialExpressionNames: string[] = Object.entries(specialExpressionTypes).reduce((acc, [name, index]) => {
  acc[index] = name
  return acc
}, [] as string[])

/**
 * Extract the symbol name from any symbol node type.
 * UserDefinedSymbolNode: node[1] is the string name
 * NormalBuiltinSymbolNode: node[1] is an index, need reverse lookup
 * SpecialBuiltinSymbolNode: node[1] is an index, need reverse lookup
 */
export function getSymbolName(symbol: SymbolNode): string {
  if (isUserDefinedSymbolNode(symbol)) {
    return symbol[1]
  }
  if (isNormalBuiltinSymbolNode(symbol)) {
    return normalExpressionNames[symbol[1]]!
  }
  // SpecialBuiltinSymbolNode
  return specialExpressionNames[symbol[1]]!
}

export function createNamedNormalExpressionNode(symbolNode: NormalBuiltinSymbolNode | UserDefinedSymbolNode, params: AstNode[], sourceCodeInfo: SourceCodeInfo | undefined): NormalExpressionNodeWithName {
  const node: NormalExpressionNodeWithName = withSourceCodeInfo([NodeTypes.NormalExpression, [symbolNode, params]], sourceCodeInfo)

  if (isNormalBuiltinSymbolNode(symbolNode)) {
    assertNumberOfParams(allNormalExpressions[symbolNode[1]]!.arity, node[1][1].length, sourceCodeInfo)
  }

  return node
}

export function isAtExpressionEnd(ctx: ParserContext): boolean {
  if (ctx.isAtEnd()) {
    return true
  }
  const token = ctx.tryPeek()
  if (isOperatorToken(token)) {
    return [';', ',', ':'].includes(token[1])
  }
  if (isReservedSymbolToken(token)) {
    return ['else', 'when', 'while', 'case', 'catch', 'let', 'then', 'end', 'do'].includes(token[1])
  }
  return false
}

export function fromBinaryOperatorToNode(operator: OperatorToken, symbolNode: SymbolNode, left: AstNode, right: AstNode, sourceCodeInfo: SourceCodeInfo | undefined): AstNode {
  const operatorName = operator[1]

  switch (operatorName) {
    case '^': // exponentiation
    case '*':
    case '/':
    case '%':
    case '+':
    case '-':
    case '<<':
    case '>>':
    case '>>>':
    case '++':
    case '<':
    case '<=':
    case '≤':
    case '>':
    case '>=':
    case '≥':
    case '==':
    case '!=':
    case '≠':
    case '&':
    case 'xor':
    case '|':
    case '|>':
      return createNamedNormalExpressionNode(symbolNode as NormalBuiltinSymbolNode, [left, right], sourceCodeInfo)
    case '&&':
    case '||':
    case '??':
      return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes[operatorName], [left, right]]] as AndNode, sourceCodeInfo)
    /* v8 ignore next 11 */
    case '.':
    case ';':
    case ':':
    case '=':
    case ',':
    case '->':
    case '...':
    case '?':
      throw new LitsError(`Unknown binary operator: ${operatorName}`, sourceCodeInfo)
    default:
      throw new LitsError(`Unknown binary operator: ${operatorName satisfies never}`, sourceCodeInfo)
  }
}
