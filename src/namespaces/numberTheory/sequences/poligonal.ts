import { assertFunctionLike } from '../../../typeGuards/lits'
import { assertNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import type { SequenceNormalExpressions } from '.'

export const poligonalNormalExpressions: SequenceNormalExpressions<'polygonal'> = {
  'polygonal-seq': {
    evaluate: ([sides, n], sourceCodeInfo): number[] => {
      assertNumber(sides, sourceCodeInfo, { integer: true, gte: 3 })
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })

      const polygonal = []
      for (let i = 1; i <= n; i += 1) {
        polygonal[i - 1] = (i * i * (sides - 2) - i * (sides - 4)) / 2
      }
      return polygonal
    },
    arity: toFixedArity(2),
  },
  'polygonal-take-while': {
    evaluate: ([sides, fn], sourceCodeInfo, contextStack, { executeFunction }): number[] => {
      assertNumber(sides, sourceCodeInfo, { integer: true, gte: 3 })
      assertFunctionLike(fn, sourceCodeInfo)

      const polygonal = []
      for (let i = 1; ; i += 1) {
        const value = (i * i * (sides - 2) - i * (sides - 4)) / 2
        if (!executeFunction(fn, [value, i], contextStack, sourceCodeInfo)) {
          break
        }
        polygonal[i - 1] = (i * i * (sides - 2) - i * (sides - 4)) / 2
      }
      return polygonal
    },
    arity: toFixedArity(2),
  },
  'polygonal-nth': {
    evaluate: ([sides, n], sourceCodeInfo): number => {
      assertNumber(sides, sourceCodeInfo, { integer: true, gte: 3 })
      assertNumber(n, sourceCodeInfo, { integer: true, positive: true })
      return (n * n * (sides - 2) - n * (sides - 4)) / 2
    },
    arity: toFixedArity(2),
  },
  'polygonal?': {
    evaluate: ([sides, n], sourceCodeInfo): boolean => {
      assertNumber(n, sourceCodeInfo, { integer: true })
      assertNumber(sides, sourceCodeInfo, { integer: true, gte: 3 })

      if (n <= 0) {
        return false
      }
      const a = sides - 2
      const b = sides - 4

      const discriminant = 8 * a * n + b * b
      const sqrtPart = Math.sqrt(discriminant)

      // Discriminant must yield an integer square root
      if (!Number.isInteger(sqrtPart))
        return false

      const numerator = sqrtPart + b

      // Numerator must be divisible by 2*a
      if (numerator % (2 * a) !== 0)
        return false

      const x = numerator / (2 * a)

      // x must be a positive integer
      return Number.isInteger(x) && x > 0
    },
    arity: toFixedArity(2),
  },
}
