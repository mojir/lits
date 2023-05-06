import { Type } from '../../../types/Type'
import { Arr } from '../../../interface'
import { array, assertNumberOfParams, asValue, number } from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'
import { evaluateMap } from './sequence'
export const arrayNormalExpression: BuiltinNormalExpressions = {
  array: {
    evaluate: (params): Arr => params,
    validateArity: () => undefined,
  },

  range: {
    evaluate: (params, debugInfo): Arr | Type => {
      if (params.every(param => !(param instanceof Type))) {
        const [first, second, third] = params
        let from: number
        let to: number
        let step: number
        number.assert(first, debugInfo, { finite: true })

        if (params.length === 1) {
          from = 0
          to = first
          step = to >= 0 ? 1 : -1
        } else if (params.length === 2) {
          number.assert(second, debugInfo, { finite: true })
          from = first
          to = second
          step = to >= from ? 1 : -1
        } else {
          number.assert(second, debugInfo, { finite: true })
          number.assert(third, debugInfo, { finite: true })
          from = first
          to = second
          step = third
          if (to > from) {
            number.assert(step, debugInfo, { positive: true })
          } else if (to < from) {
            number.assert(step, debugInfo, { negative: true })
          } else {
            number.assert(step, debugInfo, { nonZero: true })
          }
        }

        const result: number[] = []

        for (let i = from; step < 0 ? i > to : i < to; i += step) {
          result.push(i)
        }

        return result
      } else {
        const paramTypes = params.map(param => {
          const type = Type.of(param)
          type.assertIs(Type.float, debugInfo)
          return type
        })
        const fromType = asValue(paramTypes[0])
        if (paramTypes.length === 1) {
          // Here we always know if it is emptyArray or nonEmptyArray
          return fromType.is(Type.zero) ? Type.emptyArray : Type.nonEmptyArray
        }
        const toType = asValue(paramTypes[1])
        // If both from and to are zero -> emptyArray, otherwise we don't know -> array
        return fromType.is(Type.zero) && toType.is(Type.zero) ? Type.emptyArray : Type.array
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 1, max: 3 }, arity, `range`, debugInfo),
  },

  repeat: {
    evaluate: (params, debugInfo): Arr | Type => {
      if (params.every(Type.isNotType)) {
        const [count, value] = params
        number.assert(count, debugInfo, { integer: true, nonNegative: true })
        const result: Arr = []
        for (let i = 0; i < count; i += 1) {
          result.push(value)
        }
        return result
      } else {
        const countType = Type.of(params[0])
        countType.assertIs(Type.nonNegativeInteger, debugInfo)
        return countType.is(Type.zero) ? Type.emptyArray : Type.nonEmptyArray
      }
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(2, arity, `repeat`, debugInfo),
  },

  flatten: {
    evaluate: ([seq]): Arr => {
      if (!array.is(seq)) {
        return []
      }
      return seq.flat(Infinity)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams(1, arity, `flatten`, debugInfo),
  },

  mapcat: {
    evaluate: (params, debugInfo, contextStack, helpers): Arr => {
      params.slice(1).forEach(arr => {
        array.assert(arr, debugInfo)
      })
      const mapResult = evaluateMap(params, debugInfo, contextStack, helpers)
      array.assert(mapResult, debugInfo)
      return mapResult.flat(1)
    },
    validateArity: (arity, debugInfo) => assertNumberOfParams({ min: 2 }, arity, `mapcat`, debugInfo),
  },
}
