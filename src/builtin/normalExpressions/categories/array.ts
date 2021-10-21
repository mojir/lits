import { Arr } from '../../../interface'
import {
  assertLength,
  assertNegativeNumber,
  assertNonNegativeInteger,
  assertFiniteNumber,
  assertNumberNotZero,
  assertPositiveNumber,
  isArr,
  assertArr,
} from '../../../utils'
import { BuiltinNormalExpressions } from '../../interface'
import { evaluateMap } from './sequence'
export const arrayNormalExpression: BuiltinNormalExpressions = {
  array: {
    evaluate: (params: Arr): Arr => params,
  },

  range: {
    evaluate: (params: Arr): Arr => {
      const [first, second, third] = params
      let from: number
      let to: number
      let step: number
      assertFiniteNumber(first)

      if (params.length === 1) {
        from = 0
        to = first
        step = to >= 0 ? 1 : -1
      } else if (params.length === 2) {
        assertFiniteNumber(second)
        from = first
        to = second
        step = to >= from ? 1 : -1
      } else {
        assertFiniteNumber(second)
        assertFiniteNumber(third)
        from = first
        to = second
        step = third
        if (to > from) {
          assertPositiveNumber(step)
        } else if (to < from) {
          assertNegativeNumber(step)
        } else {
          assertNumberNotZero(step)
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
    evaluate: ([count, value]: Arr): Arr => {
      assertNonNegativeInteger(count)
      const result: Arr = []
      for (let i = 0; i < count; i += 1) {
        result.push(value)
      }
      return result
    },
    validate: node => assertLength(2, node),
  },

  flatten: {
    evaluate: ([seq]: Arr): Arr => {
      if (!isArr(seq)) {
        return []
      }
      return seq.flat(Number.POSITIVE_INFINITY)
    },
    validate: node => assertLength(1, node),
  },
  mapcat: {
    evaluate: (params, contextStack, helpers): Arr | string => {
      params.slice(1).forEach(arr => {
        assertArr(arr)
      })
      const mapResult = evaluateMap(params, contextStack, helpers)
      assertArr(mapResult)
      return mapResult.flat(1)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
}
