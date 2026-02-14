import { LitsError } from '../../../errors'
import { assertNonEmptyVector, assertVector } from '../../../typeGuards/annotatedArrays'
import { assertFunctionLike } from '../../../typeGuards/lits'
import { assertNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import type { BuiltinNormalExpressions } from '../../../builtin/interface'
import type { LitsNamespace } from '../interface'
import { bincount } from './bincount'
import { calcHistogram } from './histogram'
import { mode } from './mode'
import { hasOutliers, outliers } from './outliers'
import { calcPercentile } from './percentile'
import { quartiles } from './quartiles'
import { reductionFunctionNormalExpressions } from './reductionFunctions'

const vectorFunctions: BuiltinNormalExpressions = {
  'monotonic?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val >= vector[i - 1]!)
        || vector.every((val, i) => i === 0 || val <= vector[i - 1]!)
    },
    arity: toFixedArity(1),
  },
  'strictly-monotonic?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val > vector[i - 1]!)
        || vector.every((val, i) => i === 0 || val < vector[i - 1]!)
    },
    arity: toFixedArity(1),
  },
  'increasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val >= vector[i - 1]!)
    },
    arity: toFixedArity(1),
  },
  'decreasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val <= vector[i - 1]!)
    },
    arity: toFixedArity(1),
  },
  'strictly-increasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val > vector[i - 1]!)
    },
    arity: toFixedArity(1),
  },
  'strictly-decreasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val < vector[i - 1]!)
    },
    arity: toFixedArity(1),
  },
  'mode': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      return mode(vector)
    },
    arity: toFixedArity(1),
  },
  'min-index': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val, i) => (val < vector[acc]! ? i : acc), 0)
    },
    arity: toFixedArity(1),
  },
  'max-index': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val, i) => (val > vector[acc]! ? i : acc), 0)
    },
    arity: toFixedArity(1),
  },
  'sort-indices': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return [...vector.keys()].sort((a, b) => vector[a]! - vector[b]!)
    },
    arity: toFixedArity(1),
  },
  'count-values': {
    evaluate: ([vector], sourceCodeInfo): [number, number][] => {
      assertVector(vector, sourceCodeInfo)

      const frequencyMap = new Map<number, number>()
      for (const value of vector) {
        frequencyMap.set(value, (frequencyMap.get(value) || 0) + 1)
      }
      return [...frequencyMap.entries()].sort((a, b) => {
        // First compare by count (descending)
        const countDiff = b[1] - a[1]
        if (countDiff !== 0)
          return countDiff
        // If counts are equal, sort by value (ascending)
        return a[0] - b[0]
      })
    },
    arity: toFixedArity(1),
  },
  'linspace': {
    evaluate: ([start, end, numPoints], sourceCodeInfo): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(end, sourceCodeInfo, { finite: true })
      assertNumber(numPoints, sourceCodeInfo, { integer: true, nonNegative: true })

      if (numPoints === 0) {
        return []
      }
      if (numPoints === 1) {
        return [start]
      }
      const step = (end - start) / (numPoints - 1)
      return Array.from({ length: numPoints }, (_, i) => start + i * step)
    },
    arity: toFixedArity(3),
  },
  'ones': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, nonNegative: true })
      return Array.from({ length }, () => 1)
    },
    arity: toFixedArity(1),
  },
  'zeros': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, nonNegative: true })
      return Array.from({ length }, () => 0)
    },
    arity: toFixedArity(1),
  },
  'fill': {
    evaluate: ([length, value], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, nonNegative: true })
      return Array.from({ length }, () => value) as number[]
    },
    arity: toFixedArity(2),
  },
  'generate': {
    evaluate: ([length, generator], sourceCodeInfo, contextStack, { executeFunction }): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, nonNegative: true })
      assertFunctionLike(generator, sourceCodeInfo)

      return Array.from({ length }, (_, i) => {
        const value = executeFunction(generator, [i], contextStack, sourceCodeInfo)
        assertNumber(value, sourceCodeInfo, { finite: true })
        return value
      })
    },
    arity: toFixedArity(2),
  },
  'cumsum': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => {
        const last = acc[acc.length - 1] ?? 0
        acc.push(last + val)
        return acc
      }, [] as number[])
    },
    arity: toFixedArity(1),
  },
  'cumprod': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => {
        const last = acc[acc.length - 1] ?? 1
        acc.push(last * val)
        return acc
      }, [] as number[])
    },
    arity: toFixedArity(1),
  },
  'quartiles': {
    evaluate: ([vector], sourceCodeInfo): [number, number, number] => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < 4) {
        throw new LitsError('Quartiles require at least four values', sourceCodeInfo)
      }
      return quartiles(vector)
    },
    arity: toFixedArity(1),
  },
  'percentile': {
    evaluate: ([vector, percentile], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      assertNumber(percentile, sourceCodeInfo, { finite: true, nonNegative: true, lte: 100 })
      return calcPercentile(vector, percentile)
    },
    arity: toFixedArity(2),
  },
  'quantile': {
    evaluate: ([vector, quantile], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(quantile, sourceCodeInfo, { finite: true, nonNegative: true, lte: 1 })
      return calcPercentile(vector, quantile * 100)
    },
    arity: toFixedArity(2),
  },
  'histogram': {
    evaluate: ([vector, bins], sourceCodeInfo): [number, number, number][] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(bins, sourceCodeInfo, { integer: true, positive: true })

      return calcHistogram(vector, bins)
    },
    arity: toFixedArity(2),
  },
  'ecdf': {
    evaluate: ([vector, value], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      assertNumber(value, sourceCodeInfo, { finite: true })

      const sorted = [...vector].sort((a, b) => a - b)
      const index = sorted.findIndex(val => val > value)

      return index === -1 ? 1 : index / sorted.length
    },
    arity: toFixedArity(2),
  },
  'outliers?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return hasOutliers(vector)
    },
    arity: toFixedArity(1),
  },
  'outliers': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      return outliers(vector)
    },
    arity: toFixedArity(1),
  },
  'bincount': {
    evaluate: (params, sourceCodeInfo): number[] => {
      const vector = params[0]
      assertVector(vector, sourceCodeInfo)
      vector.forEach(val => assertNumber(val, sourceCodeInfo, { finite: true, integer: true, nonNegative: true }))

      const minSize = params[1] ?? 0
      assertNumber(minSize, sourceCodeInfo, { integer: true, nonNegative: true })

      const weights = params[2] ?? undefined
      if (weights !== undefined) {
        assertVector(weights, sourceCodeInfo)
        if (weights.length !== vector.length) {
          throw new LitsError('Weights vector must be the same length as the input vector', sourceCodeInfo)
        }
        weights.forEach(val => assertNumber(val, sourceCodeInfo, { finite: true }))
      }

      return bincount(vector, minSize, weights)
    },
    arity: { min: 1, max: 3 },
  },
  'winsorize': {
    evaluate: ([vector, lowerQuantile, upperQuantile], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(lowerQuantile, sourceCodeInfo, { finite: true, gte: 0, lte: 1 })
      upperQuantile ??= lowerQuantile > 0.5 ? 1 : (1 - lowerQuantile)
      assertNumber(upperQuantile, sourceCodeInfo, { finite: true, gte: lowerQuantile, lte: 1 })

      if (vector.length === 0)
        return []

      const sorted = [...vector].sort((a, b) => a - b)

      const lowerIndex = Math.max(0, Math.floor(lowerQuantile * vector.length))
      const upperIndex = Math.min(vector.length - 1, Math.max(0, Math.floor(upperQuantile * vector.length) - 1))

      const lowerBound = sorted[lowerIndex]!
      const upperBound = sorted[upperIndex]!

      return vector.map(val => Math.max(lowerBound, Math.min(val, upperBound)))
    },
    arity: { min: 2, max: 3 },
  },
  'mse': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return vectorA.reduce((acc, val, i) => acc + (val - vectorB[i]!) ** 2, 0) / vectorA.length
    },
    arity: toFixedArity(2),
  },
  'rmse': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return Math.sqrt(vectorA.reduce((acc, val, i) => acc + (val - vectorB[i]!) ** 2, 0) / vectorA.length)
    },
    arity: toFixedArity(2),
  },
  'mae': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return vectorA.reduce((acc, val, i) => acc + Math.abs(val - vectorB[i]!), 0) / vectorA.length
    },
    arity: toFixedArity(2),
  },
  'smape': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return vectorA.reduce((acc, val, i) => {
        const diff = Math.abs(val - vectorB[i]!)
        const denom = (Math.abs(val) + Math.abs(vectorB[i]!)) / 2
        return acc + (denom === 0 ? 0 : diff / denom)
      }, 0) / vectorA.length
    },
    arity: toFixedArity(2),
  },

}

addReductionFunctions(reductionFunctionNormalExpressions)

function addReductionFunctions(sequences: BuiltinNormalExpressions) {
  for (const [key, value] of Object.entries(sequences)) {
    /* v8 ignore next 3 */
    if (vectorFunctions[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    vectorFunctions[key] = value
  }
}

export const vectorNamespace: LitsNamespace = {
  name: 'Vector',
  functions: vectorFunctions,
}
