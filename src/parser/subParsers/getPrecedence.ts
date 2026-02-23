import { LitsError } from '../../errors'
import type { SourceCodeInfo } from '../../tokenizer/token'
import type { SymbolicBinaryOperator } from '../../tokenizer/operators'
import { exponentiationPrecedence } from '../helpers'

export function getPrecedence(operatorSign: SymbolicBinaryOperator, sourceCodeInfo: SourceCodeInfo | undefined): number {
  switch (operatorSign) {
    case '^': // exponentiation
      return exponentiationPrecedence

    case '*': // multiplication
    case '/': // division
    case '%': // remainder
      return 11

    case '+': // addition
    case '-': // subtraction
      return 10

    case '<<': // left shift
    case '>>': // signed right shift
    case '>>>': // unsigned right shift
      return 9

    case '++': // string concatenation
      return 8

    case '<': // less than
    case '<=': // less than or equal
    case '≤': // less than or equal
    case '>': // greater than
    case '>=': // greater than or equal
    case '≥': // greater than or equal
      return 7

    case '==': // equal
    case '!=': // not equal
      return 6

    case '&': // bitwise AND
    case 'xor': // bitwise XOR
    case '|': // bitwise OR
      return 5

    case '&&': // logical AND
    case '||': // logical OR
    case '??': // nullish coalescing
      return 4

      // leave room for binaryFunctionalOperatorPrecedence = 3

    case '|>': // pipe
      return 2

      // leave room for conditionalOperatorPrecedence = 1

    /* v8 ignore next 2 */
    default:
      throw new LitsError(`Unknown binary operator: ${operatorSign satisfies never}`, sourceCodeInfo)
  }
}
