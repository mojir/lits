import { assertNumber } from '../../../typeGuards/number'
import { assertNumberOfParams } from '../../../typeGuards'
import type { BuiltinNormalExpressions } from '../../interface'

export const bitwiseNormalExpression: BuiltinNormalExpressions = {
  '<<': {
    evaluate: ([num, count], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return num << count
    },
    validate: node => assertNumberOfParams(2, node),
  },
  '>>': {
    evaluate: ([num, count], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return num >> count
    },
    validate: node => assertNumberOfParams(2, node),
  },
  '>>>': {
    evaluate: ([num, count], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })

      return num >>> count
    },
    validate: node => assertNumberOfParams(2, node),
  },
  '~': {
    evaluate: ([num], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      return ~num
    },
    validate: node => assertNumberOfParams(1, node),
  },
  '&': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result & value
      }, first)
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  '&!': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result & ~value
      }, first)
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  '|': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result | value
      }, first)
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  '^': {
    evaluate: ([first, ...rest], sourceCodeInfo): number => {
      assertNumber(first, sourceCodeInfo, { integer: true })

      return rest.reduce((result: number, value) => {
        assertNumber(value, sourceCodeInfo, { integer: true })
        return result ^ value
      }, first)
    },
    validate: node => assertNumberOfParams({ min: 2 }, node),
  },
  'bit_flip': {
    evaluate: ([num, index], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num ^= mask)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'bit_set': {
    evaluate: ([num, index], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num |= mask)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'bit_clear': {
    evaluate: ([num, index], sourceCodeInfo): number => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return (num &= ~mask)
    },
    validate: node => assertNumberOfParams(2, node),
  },
  'bit_test': {
    evaluate: ([num, index], sourceCodeInfo): boolean => {
      assertNumber(num, sourceCodeInfo, { integer: true })
      assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true })

      const mask = 1 << index
      return !!(num & mask)
    },
    validate: node => assertNumberOfParams(2, node),
  },
}
