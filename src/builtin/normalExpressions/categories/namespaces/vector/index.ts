import { LitsError } from '../../../../../errors'
import { assertNonEmptyVector, assertVector } from '../../../../../typeGuards/annotatedArrays'
import { assertFunctionLike } from '../../../../../typeGuards/lits'
import { assertNumber } from '../../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../../interface'
import { bincount } from './bincount'
import { calcHistogram } from './histogram'
import { mode } from './mode'
import { hasOutliers, outliers } from './outliers'
import { calcPercentile } from './percentile'
import { quartiles } from './quartiles'
import { reductionFunctionNormalExpressions } from './reductionFunctions'

export const vectorNormalExpression: BuiltinNormalExpressions = {
  'vec:monotonic?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val >= vector[i - 1]!)
        || vector.every((val, i) => i === 0 || val <= vector[i - 1]!)
    },
    paramCount: 1,
  },
  'vec:strictly-monotonic?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val > vector[i - 1]!)
        || vector.every((val, i) => i === 0 || val < vector[i - 1]!)
    },
    paramCount: 1,
  },
  'vec:increasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val >= vector[i - 1]!)
    },
    paramCount: 1,
  },
  'vec:decreasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val <= vector[i - 1]!)
    },
    paramCount: 1,
  },
  'vec:strictly-increasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val > vector[i - 1]!)
    },
    paramCount: 1,
  },
  'vec:strictly-decreasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val < vector[i - 1]!)
    },
    paramCount: 1,
  },
  'vec:mode': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      return mode(vector)
    },
    paramCount: 1,
  },
  'vec:min-index': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val, i) => (val < vector[acc]! ? i : acc), 0)
    },
    paramCount: 1,
  },
  'vec:max-index': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val, i) => (val > vector[acc]! ? i : acc), 0)
    },
    paramCount: 1,
  },
  'vec:sort-indices': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return [...vector.keys()].sort((a, b) => vector[a]! - vector[b]!)
    },
    paramCount: 1,
  },
  'vec:count-values': {
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
    paramCount: 1,
  },
  'vec:linspace': {
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
    paramCount: 3,
  },
  'vec:ones': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, nonNegative: true })
      return Array.from({ length }, () => 1)
    },
    paramCount: 1,
  },
  'vec:zeros': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, nonNegative: true })
      return Array.from({ length }, () => 0)
    },
    paramCount: 1,
  },
  'vec:fill': {
    evaluate: ([length, value], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, nonNegative: true })
      return Array.from({ length }, () => value) as number[]
    },
    paramCount: 2,
  },
  'vec:generate': {
    evaluate: ([length, generator], sourceCodeInfo, contextStack, { executeFunction }): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, nonNegative: true })
      assertFunctionLike(generator, sourceCodeInfo)

      return Array.from({ length }, (_, i) => {
        const value = executeFunction(generator, [i], contextStack, sourceCodeInfo)
        assertNumber(value, sourceCodeInfo, { finite: true })
        return value
      })
    },
    paramCount: 2,
  },
  'vec:cumsum': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => {
        const last = acc[acc.length - 1] ?? 0
        acc.push(last + val)
        return acc
      }, [] as number[])
    },
    paramCount: 1,
  },
  'vec:cumprod': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => {
        const last = acc[acc.length - 1] ?? 1
        acc.push(last * val)
        return acc
      }, [] as number[])
    },
    paramCount: 1,
  },
  'vec:quartiles': {
    evaluate: ([vector], sourceCodeInfo): [number, number, number] => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < 4) {
        throw new LitsError('Quartiles require at least four values', sourceCodeInfo)
      }
      return quartiles(vector)
    },
    paramCount: 1,
  },
  'vec:percentile': {
    evaluate: ([vector, percentile], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      assertNumber(percentile, sourceCodeInfo, { finite: true, nonNegative: true, lte: 100 })
      return calcPercentile(vector, percentile)
    },
    paramCount: 2,
  },
  'vec:quantile': {
    evaluate: ([vector, quantile], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(quantile, sourceCodeInfo, { finite: true, nonNegative: true, lte: 1 })
      return calcPercentile(vector, quantile * 100)
    },
    paramCount: 2,
  },
  'vec:histogram': {
    evaluate: ([vector, bins], sourceCodeInfo): [number, number, number][] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(bins, sourceCodeInfo, { integer: true, positive: true })

      return calcHistogram(vector, bins)
    },
    paramCount: 2,
  },
  'vec:ecdf': {
    evaluate: ([vector, value], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      assertNumber(value, sourceCodeInfo, { finite: true })

      const sorted = [...vector].sort((a, b) => a - b)
      const index = sorted.findIndex(val => val > value)

      return index === -1 ? 1 : index / sorted.length
    },
    paramCount: 2,
  },
  'vec:outliers?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return hasOutliers(vector)
    },
    paramCount: 1,
  },
  'vec:outliers': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      return outliers(vector)
    },
    paramCount: 1,
  },
  'vec:bincount': {
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
    paramCount: { min: 1, max: 3 },
  },
  'vec:winsorize': {
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
    paramCount: { min: 2, max: 3 },
  },
  'vec:mse': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return vectorA.reduce((acc, val, i) => acc + (val - vectorB[i]!) ** 2, 0) / vectorA.length
    },
    paramCount: 2,
  },
  'vec:rmse': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return Math.sqrt(vectorA.reduce((acc, val, i) => acc + (val - vectorB[i]!) ** 2, 0) / vectorA.length)
    },
    paramCount: 2,
  },
  'vec:mae': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertNonEmptyVector(vectorA, sourceCodeInfo)
      assertNonEmptyVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return vectorA.reduce((acc, val, i) => acc + Math.abs(val - vectorB[i]!), 0) / vectorA.length
    },
    paramCount: 2,
  },
  'vec:smape': {
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
    paramCount: 2,
  },

}

addReductionFunctions(reductionFunctionNormalExpressions)

function addReductionFunctions(sequences: BuiltinNormalExpressions) {
  for (const [key, value] of Object.entries(sequences)) {
    /* v8 ignore next 3 */
    if (vectorNormalExpression[key]) {
      throw new Error(`Duplicate normal expression key found: ${key}`)
    }
    vectorNormalExpression[key] = value
  }
}
