import { assertInteger, assertLength, assertNonNegativeInteger } from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'

export const bitwiseNormalExpression: BuiltinNormalExpressions = {
  'bit-shift-left': {
    evaluate: ([number, count], meta): number => {
      assertInteger(number, meta)
      assertNonNegativeInteger(count, meta)

      return number << count
    },
    validate: node => assertLength(2, node),
  },
  'bit-shift-right': {
    evaluate: ([number, count], meta): number => {
      assertInteger(number, meta)
      assertNonNegativeInteger(count, meta)

      return number >> count
    },
    validate: node => assertLength(2, node),
  },
  'bit-not': {
    evaluate: ([number], meta): number => {
      assertInteger(number, meta)
      return ~number
    },
    validate: node => assertLength(1, node),
  },
  'bit-and': {
    evaluate: ([first, ...rest], meta): number => {
      assertInteger(first, meta)

      return rest.reduce((result: number, value) => {
        assertInteger(value, meta)
        return result & value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-and-not': {
    evaluate: ([first, ...rest], meta): number => {
      assertInteger(first, meta)

      return rest.reduce((result: number, value) => {
        assertInteger(value, meta)
        return result & ~value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-or': {
    evaluate: ([first, ...rest], meta): number => {
      assertInteger(first, meta)

      return rest.reduce((result: number, value) => {
        assertInteger(value, meta)
        return result | value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-xor': {
    evaluate: ([first, ...rest], meta): number => {
      assertInteger(first, meta)

      return rest.reduce((result: number, value) => {
        assertInteger(value, meta)
        return result ^ value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-flip': {
    evaluate: ([number, index], meta): number => {
      assertInteger(number, meta)
      assertNonNegativeInteger(index, meta)

      const mask = 1 << index
      return (number ^= mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-set': {
    evaluate: ([number, index], meta): number => {
      assertInteger(number, meta)
      assertNonNegativeInteger(index, meta)

      const mask = 1 << index
      return (number |= mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-clear': {
    evaluate: ([number, index], meta): number => {
      assertInteger(number, meta)
      assertNonNegativeInteger(index, meta)

      const mask = 1 << index
      return (number &= ~mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-test': {
    evaluate: ([number, index], meta): boolean => {
      assertInteger(number, meta)
      assertNonNegativeInteger(index, meta)

      const mask = 1 << index
      return !!(number & mask)
    },
    validate: node => assertLength(2, node),
  },
}
