import { Arr } from '../../../interface'
import {
  assertLength,
  assertNegativeNumber,
  assertNonNegativeInteger,
  assertFiniteNumber,
  assertNumberNotZero,
  assertPositiveNumber,
} from '../../../utils'
import { array } from '../../../utils/assertion'
import { BuiltinNormalExpressions } from '../../interface'
import { evaluateMap } from './sequence'
export const arrayNormalExpression: BuiltinNormalExpressions = {
  array: {
    evaluate: (params): Arr => params,
  },

  range: {
    evaluate: (params, sourceCodeInfo): Arr => {
      const [first, second, third] = params
      let from: number
      let to: number
      let step: number
      assertFiniteNumber(first, sourceCodeInfo)

      if (params.length === 1) {
        from = 0
        to = first
        step = to >= 0 ? 1 : -1
      } else if (params.length === 2) {
        assertFiniteNumber(second, sourceCodeInfo)
        from = first
        to = second
        step = to >= from ? 1 : -1
      } else {
        assertFiniteNumber(second, sourceCodeInfo)
        assertFiniteNumber(third, sourceCodeInfo)
        from = first
        to = second
        step = third
        if (to > from) {
          assertPositiveNumber(step, sourceCodeInfo)
        } else if (to < from) {
          assertNegativeNumber(step, sourceCodeInfo)
        } else {
          assertNumberNotZero(step, sourceCodeInfo)
        }
      }

      const result: number[] = []

      for (let i = from; step < 0 ? i > to : i < to; i += step) {
        result.push(i)
      }

      return result
    },
    validate: node => assertLength({ min: 1, max: 3 }, node),
  },

  repeat: {
    evaluate: ([count, value], sourceCodeInfo): Arr => {
      assertNonNegativeInteger(count, sourceCodeInfo)
      const result: Arr = []
      for (let i = 0; i < count; i += 1) {
        result.push(value)
      }
      return result
    },
    validate: node => assertLength(2, node),
  },

  flatten: {
    evaluate: ([seq]): Arr => {
      if (!array.is(seq)) {
        return []
      }
      return seq.flat(Number.POSITIVE_INFINITY)
    },
    validate: node => assertLength(1, node),
  },
  mapcat: {
    evaluate: (params, sourceCodeInfo, contextStack, helpers): Arr | string => {
      params.slice(1).forEach(arr => {
        array.assert(arr, sourceCodeInfo)
      })
      const mapResult = evaluateMap(params, sourceCodeInfo, contextStack, helpers)
      array.assert(mapResult, sourceCodeInfo)
      return mapResult.flat(1)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
}
