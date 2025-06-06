import { assertNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import type { BuiltinNormalExpressions } from '../../interface'

export const bitwiseNormalExpression: BuiltinNormalExpressions = {
  '<<': {
    evaluate: ([num, count], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return num << count
    },
    arity: toFixedArity(2),
  },
  '>>': {
    evaluate: ([num, count], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return num >> count
    },
    arity: toFixedArity(2),
  },
  '>>>': {
    evaluate: ([num, count], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return num >>> count
    },
    arity: toFixedArity(2),
  },
  'bit-not': {
    evaluate: ([num], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      return ~num
    },
    arity: toFixedArity(1),
  },
  '&': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result & value
      }, first)
    },
    arity: { min: 2 },
  },
  'bit-and-not': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result & ~value
      }, first)
    },
    arity: { min: 2 },
  },
  '|': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result | value
      }, first)
    },
    arity: { min: 2 },
  },
  'xor': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result ^ value
      }, first)
    },
    arity: { min: 2 },
  },
  'bit-flip': {
    evaluate: ([num, index], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num ^= mask)
    },
    arity: toFixedArity(2),
  },
  'bit-set': {
    evaluate: ([num, index], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num |= mask)
    },
    arity: toFixedArity(2),
  },
  'bit-clear': {
    evaluate: ([num, index], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num &= ~mask)
    },
    arity: toFixedArity(2),
  },
  'bit-test': {
    evaluate: ([num, index], sourceCodeInfo): boolean => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return !!(num & mask)
    },
    arity: toFixedArity(2),
  },
}
