import { assertNumberOfParams } from '../../../utils/assertion'
import { number } from '../../../utils/numberAssertion'
import { BuiltinNormalExpressions } from '../../interface'

export const bitwiseNormalExpression: BuiltinNormalExpressions = {
  'bit-shift-left': {
    evaluate: ([num, count], debugInfo): number => {
      number.assert(num, debugInfo, { integer: true })
      number.assert(count, debugInfo, { integer: true, nonNegative: true })

      return num << count
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `bit-shift-left`, debugInfo),
  },
  'bit-shift-right': {
    evaluate: ([num, count], debugInfo): number => {
      number.assert(num, debugInfo, { integer: true })
      number.assert(count, debugInfo, { integer: true, nonNegative: true })

      return num >> count
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `bit-shift-right`, debugInfo),
  },
  'bit-not': {
    evaluate: ([num], debugInfo): number => {
      number.assert(num, debugInfo, { integer: true })
      return ~num
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `bit-not`, debugInfo),
  },
  'bit-and': {
    evaluate: ([first, ...rest], debugInfo): number => {
      number.assert(first, debugInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        number.assert(value, debugInfo, { integer: true })
        return result & value
      }, first)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2 }, arity, `bit-and`, debugInfo),
  },
  'bit-and-not': {
    evaluate: ([first, ...rest], debugInfo): number => {
      number.assert(first, debugInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        number.assert(value, debugInfo, { integer: true })
        return result & ~value
      }, first)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2 }, arity, `bit-and-not`, debugInfo),
  },
  'bit-or': {
    evaluate: ([first, ...rest], debugInfo): number => {
      number.assert(first, debugInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        number.assert(value, debugInfo, { integer: true })
        return result | value
      }, first)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2 }, arity, `bit-or`, debugInfo),
  },
  'bit-xor': {
    evaluate: ([first, ...rest], debugInfo): number => {
      number.assert(first, debugInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        number.assert(value, debugInfo, { integer: true })
        return result ^ value
      }, first)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2 }, arity, `bit-xor`, debugInfo),
  },
  'bit-flip': {
    evaluate: ([num, index], debugInfo): number => {
      number.assert(num, debugInfo, { integer: true })
      number.assert(index, debugInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num ^= mask)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `bit-flip`, debugInfo),
  },
  'bit-set': {
    evaluate: ([num, index], debugInfo): number => {
      number.assert(num, debugInfo, { integer: true })
      number.assert(index, debugInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num |= mask)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `bit-set`, debugInfo),
  },
  'bit-clear': {
    evaluate: ([num, index], debugInfo): number => {
      number.assert(num, debugInfo, { integer: true })
      number.assert(index, debugInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num &= ~mask)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `bit-clear`, debugInfo),
  },
  'bit-test': {
    evaluate: ([num, index], debugInfo): boolean => {
      number.assert(num, debugInfo, { integer: true })
      number.assert(index, debugInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return !!(num & mask)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `bit-test`, debugInfo),
  },
}
