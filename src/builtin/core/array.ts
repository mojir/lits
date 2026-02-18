import type { Arr } from '../../interface'
import { assertArray } from '../../typeGuards/array'
import { asNumber, assertNumber } from '../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../interface'
import { assertFunctionLike } from '../../typeGuards/lits'
import { toFixedArity } from '../../utils/arity'

export const arrayNormalExpression: BuiltinNormalExpressions = {
  'range': {
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
    arity: { min: 1, max: 3 },
  },

  'repeat': {
    evaluate: ([value, count], sourceCodeInfo): Arr => {
      assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true })
      const result: Arr = []
      for (let i = 0; i < count; i += 1)
        result.push(value)

      return result
    },
    arity: toFixedArity(2),
  },

  'flatten': {
    evaluate: ([seq, depth], sourceCodeInfo): Arr => {
      assertArray(seq, sourceCodeInfo)

      const actualDepth = depth === undefined || depth === Number.POSITIVE_INFINITY
        ? Number.POSITIVE_INFINITY
        : asNumber(depth, sourceCodeInfo, { integer: true, nonNegative: true })

      return seq.flat(actualDepth)
    },
    arity: { min: 1, max: 2 },
  },
  'mapcat': {
    evaluate: ([arr, fn], sourceCodeInfo, contextStack, { executeFunction }): Arr | string => {
      assertArray(arr, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)
      return arr.map(elem => executeFunction(fn, [elem], contextStack, sourceCodeInfo)).flat(1)
    },
    arity: toFixedArity(2),
  },
  'moving-fn': {
    evaluate: ([arr, windowSize, fn], sourceCodeInfo, contextStack, { executeFunction }): Arr => {
      assertArray(arr, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, lte: arr.length })
      assertFunctionLike(fn, sourceCodeInfo)

      const result = []
      for (let i = 0; i <= arr.length - windowSize; i++) {
        const window = arr.slice(i, i + windowSize)
        const value = executeFunction(fn, [window], contextStack, sourceCodeInfo)
        result.push(value)
      }
      return result
    },
    arity: toFixedArity(3),
  },
  'running-fn': {
    evaluate: ([arr, fn], sourceCodeInfo, contextStack, { executeFunction }): Arr => {
      assertArray(arr, sourceCodeInfo)
      assertFunctionLike(fn, sourceCodeInfo)

      const result = []
      for (let i = 0; i < arr.length; i += 1) {
        const subArr = arr.slice(0, i + 1)
        result.push(executeFunction(fn, [subArr], contextStack, sourceCodeInfo))
      }
      return result
    },
    arity: toFixedArity(2),
  },
}
