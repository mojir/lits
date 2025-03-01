import type { Arr } from '../../../interface'
import { assertArray } from '../../../typeGuards/array'
import { assertNumber } from '../../../typeGuards/number'
import { assertNumberOfParams } from '../../../typeGuards'
import type { BuiltinNormalExpressions } from '../../interface'
import { assertLitsFunction } from '../../../typeGuards/litsFunction'

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
      assertNumber(first, sourceCodeInfo, { finite: true })

      if (params.length === 1) {
        from = 0
        to = first
        step = to >= 0 ? 1 : -1
      }
      else if (params.length === 2) {
        assertNumber(second, sourceCodeInfo, { finite: true })
        from = first
        to = second
        step = to >= from ? 1 : -1
      }
      else {
        assertNumber(second, sourceCodeInfo, { finite: true })
        assertNumber(third, sourceCodeInfo, { finite: true })
        from = first
        to = second
        step = third
        if (to > from)
          assertNumber(step, sourceCodeInfo, { positive: true })
        else if (to < from)
          assertNumber(step, sourceCodeInfo, { negative: true })
        else
          assertNumber(step, sourceCodeInfo, { nonZero: true })
      }

      const result: number[] = []

      for (let i = from; step < 0 ? i > to : i < to; i += step)
        result.push(i)

      return result
    },
    validate: node => assertNumberOfParams({ min: 1, max: 3 }, node),
  },

  repeat: {
    evaluate: ([value, count], sourceCodeInfo): Arr => {
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })
      const result: Arr = []
      for (let i = 0; i < count; i += 1)
        result.push(value)

      return result
    },
    validate: node => assertNumberOfParams(2, node),
  },

  flatten: {
    evaluate: ([seq]): Arr => {
      if (!Array.isArray(seq))
        return []

      return seq.flat(Number.POSITIVE_INFINITY)
    },
    validate: node => assertNumberOfParams(1, node),
  },
  mapcat: {
    evaluate: ([arr, fn], sourceCodeInfo, contextStack, { executeFunction }): Arr | string => {
      assertArray(arr, sourceCodeInfo)
      assertLitsFunction(fn, sourceCodeInfo)
      return arr.map(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo)).flat(1)
    },
    validate: node => assertNumberOfParams(2, node),
  },
}
