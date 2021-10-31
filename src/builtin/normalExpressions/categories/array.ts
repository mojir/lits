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
    evaluate: (params): Arr => params,
  },

  range: {
    evaluate: (params, meta): Arr => {
      const [first, second, third] = params
      let from: number
      let to: number
      let step: number
      assertFiniteNumber(first, meta)

      if (params.length === 1) {
        from = 0
        to = first
        step = to >= 0 ? 1 : -1
      } else if (params.length === 2) {
        assertFiniteNumber(second, meta)
        from = first
        to = second
        step = to >= from ? 1 : -1
      } else {
        assertFiniteNumber(second, meta)
        assertFiniteNumber(third, meta)
        from = first
        to = second
        step = third
        if (to > from) {
          assertPositiveNumber(step, meta)
        } else if (to < from) {
          assertNegativeNumber(step, meta)
        } else {
          assertNumberNotZero(step, meta)
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
    evaluate: ([count, value], meta): Arr => {
      assertNonNegativeInteger(count, meta)
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
      if (!isArr(seq)) {
        return []
      }
      return seq.flat(Number.POSITIVE_INFINITY)
    },
    validate: node => assertLength(1, node),
  },
  mapcat: {
    evaluate: (params, meta, contextStack, helpers): Arr | string => {
      params.slice(1).forEach(arr => {
        assertArr(arr, meta)
      })
      const mapResult = evaluateMap(params, meta, contextStack, helpers)
      assertArr(mapResult, meta)
      return mapResult.flat(1)
    },
    validate: node => assertLength({ min: 2 }, node),
  },
}
