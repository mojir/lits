import { assertInteger, assertLength, assertNonNegativeInteger } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const bitwiseNormalExpression: BuiltinNormalExpressions = {
  'bit-shift-left': {
    evaluate: ([number, count]): number => {
      assertInteger(number)
      assertNonNegativeInteger(count)

      return number << count
    },
    validate: node => assertLength(2, node),
  },
  'bit-shift-right': {
    evaluate: ([number, count]): number => {
      assertInteger(number)
      assertNonNegativeInteger(count)

      return number >> count
    },
    validate: node => assertLength(2, node),
  },
  'bit-not': {
    evaluate: ([number]): number => {
      assertInteger(number)
      return ~number
    },
    validate: node => assertLength(1, node),
  },
  'bit-and': {
    evaluate: ([first, ...rest]): number => {
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result & value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-and-not': {
    evaluate: ([first, ...rest]): number => {
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result & ~value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-or': {
    evaluate: ([first, ...rest]): number => {
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result | value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-xor': {
    evaluate: ([first, ...rest]): number => {
      assertInteger(first)

      return rest.reduce((result: number, value) => {
        assertInteger(value)
        return result ^ value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-flip': {
    evaluate: ([number, index]): number => {
      assertInteger(number)
      assertNonNegativeInteger(index)

      const mask = 1 << index
      return (number ^= mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-set': {
    evaluate: ([number, index]): number => {
      assertInteger(number)
      assertNonNegativeInteger(index)

      const mask = 1 << index
      return (number |= mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-clear': {
    evaluate: ([number, index]): number => {
      assertInteger(number)
      assertNonNegativeInteger(index)

      const mask = 1 << index
      return (number &= ~mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-test': {
    evaluate: ([number, index]): boolean => {
      assertInteger(number)
      assertNonNegativeInteger(index)

      const mask = 1 << index
      return !!(number & mask)
    },
    validate: node => assertLength(2, node),
  },
}
