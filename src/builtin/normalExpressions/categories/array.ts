import { Arr } from '../../../interface'
import { assertLength } from '../../../utils'
import { array, number } from '../../../utils/assertion'
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
      number.assert(first, sourceCodeInfo, { finite: true })

      if (params.length === 1) {
        from = 0
        to = first
        step = to >= 0 ? 1 : -1
      } else if (params.length === 2) {
        number.assert(second, sourceCodeInfo, { finite: true })
        from = first
        to = second
        step = to >= from ? 1 : -1
      } else {
        number.assert(second, sourceCodeInfo, { finite: true })
        number.assert(third, sourceCodeInfo, { finite: true })
        from = first
        to = second
        step = third
        if (to > from) {
          number.assert(step, sourceCodeInfo, { positive: true })
        } else if (to < from) {
          number.assert(step, sourceCodeInfo, { negative: true })
        } else {
          number.assert(step, sourceCodeInfo, { nonZero: true })
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
      number.assert(count, sourceCodeInfo, { integer: true, nonNegative: true })
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
