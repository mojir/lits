import { assertInteger, assertLength, assertNonNegativeNumber } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const bitwiseNormalExpression: BuiltinNormalExpressions = {
  'bit-shift-left': {
    evaluate: ([number, count]: unknown[]): number => {
      assertInteger(number)
      assertInteger(count)
      assertNonNegativeNumber(count)

      return number << count
    },
    validate: node => assertLength(2, node),
  },
  'bit-shift-right': {
    evaluate: ([number, count]: unknown[]): number => {
      assertInteger(number)
      assertInteger(count)
      assertNonNegativeNumber(count)

      return number >> count
    },
    validate: node => assertLength(2, node),
  },
  'bit-not': {
    evaluate: ([number]: unknown[]): number => {
      assertInteger(number)
      return ~number
    },
    validate: node => assertLength(1, node),
  },
  'bit-and': {
    evaluate: ([first, ...rest]: unknown[]): number => {
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result & value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-and-not': {
    evaluate: ([first, ...rest]: unknown[]): number => {
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result & ~value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-or': {
    evaluate: ([first, ...rest]: unknown[]): number => {
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result | value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-xor': {
    evaluate: ([first, ...rest]: unknown[]): number => {
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result ^ value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-flip': {
    evaluate: ([number, index]: unknown[]): number => {
      assertInteger(number)
      assertInteger(index)
      assertNonNegativeNumber(index)

      const mask = 1 << index
      return (number ^= mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-set': {
    evaluate: ([number, index]: unknown[]): number => {
      assertInteger(number)
      assertInteger(index)
      assertNonNegativeNumber(index)

      const mask = 1 << index
      return (number |= mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-clear': {
    evaluate: ([number, index]: unknown[]): number => {
      assertInteger(number)
      assertInteger(index)
      assertNonNegativeNumber(index)

      const mask = 1 << index
      return (number &= ~mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-test': {
    evaluate: ([number, index]: unknown[]): boolean => {
      assertInteger(number)
      assertInteger(index)
      assertNonNegativeNumber(index)

      const mask = 1 << index
      return !!(number & mask)
    },
    validate: node => assertLength(2, node),
  },
}
