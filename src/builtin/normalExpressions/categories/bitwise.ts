import { assertLength, assertNonNegativeInteger } from '../../../utils'
import { number } from '../../../utils/numberAssertion'
import { BuiltinNormalExpressions } from '../../interface'

export const bitwiseNormalExpression: BuiltinNormalExpressions = {
  'bit-shift-left': {
    evaluate: ([num, count], meta): number => {
      number.assert(num, meta, { integer: true })
      assertNonNegativeInteger(count, meta)

      return num << count
    },
    validate: node => assertLength(2, node),
  },
  'bit-shift-right': {
    evaluate: ([num, count], meta): number => {
      number.assert(num, meta, { integer: true })
      assertNonNegativeInteger(count, meta)

      return num >> count
    },
    validate: node => assertLength(2, node),
  },
  'bit-not': {
    evaluate: ([num], meta): number => {
      number.assert(num, meta, { integer: true })
      return ~num
    },
    validate: node => assertLength(1, node),
  },
  'bit-and': {
    evaluate: ([first, ...rest], meta): number => {
      number.assert(first, meta, { integer: true })

      return rest.reduce((result: number, value) => {
        number.assert(value, meta, { integer: true })
        return result & value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-and-not': {
    evaluate: ([first, ...rest], meta): number => {
      number.assert(first, meta, { integer: true })

      return rest.reduce((result: number, value) => {
        number.assert(value, meta, { integer: true })
        return result & ~value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-or': {
    evaluate: ([first, ...rest], meta): number => {
      number.assert(first, meta, { integer: true })

      return rest.reduce((result: number, value) => {
        number.assert(value, meta, { integer: true })
        return result | value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-xor': {
    evaluate: ([first, ...rest], meta): number => {
      number.assert(first, meta, { integer: true })

      return rest.reduce((result: number, value) => {
        number.assert(value, meta, { integer: true })
        return result ^ value
      }, first)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
  'bit-flip': {
    evaluate: ([num, index], meta): number => {
      number.assert(num, meta, { integer: true })
      assertNonNegativeInteger(index, meta)

      const mask = 1 << index
      return (num ^= mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-set': {
    evaluate: ([num, index], meta): number => {
      number.assert(num, meta, { integer: true })
      assertNonNegativeInteger(index, meta)

      const mask = 1 << index
      return (num |= mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-clear': {
    evaluate: ([num, index], meta): number => {
      number.assert(num, meta, { integer: true })
      assertNonNegativeInteger(index, meta)

      const mask = 1 << index
      return (num &= ~mask)
    },
    validate: node => assertLength(2, node),
  },
  'bit-test': {
    evaluate: ([num, index], meta): boolean => {
      number.assert(num, meta, { integer: true })
      assertNonNegativeInteger(index, meta)

      const mask = 1 << index
      return !!(num & mask)
    },
    validate: node => assertLength(2, node),
  },
}
