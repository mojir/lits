import type { AndNode } from '../builtin/specialExpressions/and'
import { specialExpressionTypes } from '../builtin/specialExpressionTypes'
import { NodeTypes } from '../constants/constants'
import { LitsError } from '../errors'
import type { Node, NormalBuiltinSymbolNode, SymbolNode } from '../parser/types'
import type { OperatorToken, SourceCodeInfo } from '../tokenizer/token'
import { createNamedNormalExpressionNode, withSourceCodeInfo } from './helpers'

export function fromBinaryOperatorToNode(operator: OperatorToken, symbolNode: SymbolNode, left: Node, right: Node, sourceCodeInfo: SourceCodeInfo | undefined): Node {
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
