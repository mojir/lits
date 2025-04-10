import { LitsError } from '../../../../../errors'
import { assertNonEmptyVector, assertVector, isVector } from '../../../../../typeGuards/annotatedArrays'
import { assertFunctionLike } from '../../../../../typeGuards/lits'
import { assertNumber } from '../../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../../interface'
import { bincount } from './bincount'
import { calcMad } from './calcMad'
import { calcMean } from './calcMean'
import { calcMedad } from './calcMedad'
import { calcMedian } from './calcMedian'
import { calcStdDev } from './calcStdDev'
import { calcVariance } from './calcVariance'
import { calculateEntropy } from './entropy'
import { calcHistogram } from './histogram'
import { excessKurtosis, kurtosis, sampleExcessKurtosis, sampleKurtosis } from './kurtosis'
import { mode } from './mode'
import { hasOutliers, outliers } from './outliers'
import { calcPercentile } from './percentile'
import { quartiles } from './quartiles'
import { reductionFunctionNormalExpressions } from './reductionFunctions'
import { sampleSkewness, skewness } from './skewness'

export const vectorNormalExpression: BuiltinNormalExpressions = {
  'vec:vector?': {
    evaluate: ([vector]): boolean => isVector(vector),
    paramCount: 1,
  },
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
  'vec:median': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return calcMedian(vector)
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
  'vec:variance': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      const mean = calcMean(vector)
      return vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / vector.length
    },
    paramCount: 1,
  },
  'vec:sample-variance': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      if (vector.length < 2) {
        throw new LitsError('Sample variance requires at least two values', sourceCodeInfo)
      }
      const mean = calcMean(vector)
      return vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / (vector.length - 1)
    },
    paramCount: 1,
  },
  'vec:stdev': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      const mean = vector.reduce((acc, val) => acc + val, 0) / vector.length

      // calculate the squared differences from the mean, average them, and take the square root
      return Math.sqrt(
        vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / vector.length,
      )
    },
    paramCount: 1,
  },
  'vec:sample-stdev': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      if (vector.length < 2) {
        throw new LitsError('Sample standard deviation requires at least two values', sourceCodeInfo)
      }
      const mean = vector.reduce((acc, val) => acc + val, 0) / vector.length

      // calculate the squared differences from the mean, sum them, divide by (n-1), and take the square root
      return Math.sqrt(
        vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / (vector.length - 1),
      )
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
  'vec:iqr': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < 4) {
        throw new LitsError('IQR requires at least four values', sourceCodeInfo)
      }
      const [q1, , q3] = quartiles(vector)
      return q3 - q1
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
  'vec:span': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      const min = vector.reduce((acc, val) => (val < acc ? val : acc), vector[0]!)
      const max = vector.reduce((acc, val) => (val > acc ? val : acc), vector[0]!)

      return max - min
    },
    paramCount: 1,
  },
  'vec:skewness': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      if (vector.length < 3) {
        throw new LitsError('Skewness requires at least three values', sourceCodeInfo)
      }
      try {
        return skewness(vector)
      }
      catch (error) {
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: 1,
  },
  'vec:sample-skewness': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      if (vector.length < 3) {
        throw new LitsError('Skewness requires at least three values', sourceCodeInfo)
      }
      try {
        return sampleSkewness(vector)
      }
      catch (error) {
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: 1,
  },
  'vec:kurtosis': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < 4) {
        throw new LitsError('Kurtosis requires at least four values', sourceCodeInfo)
      }
      try {
        return kurtosis(vector)
      }
      catch (error) {
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: 1,
  },
  'vec:excess-kurtosis': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < 4) {
        throw new LitsError('Kurtosis requires at least four values', sourceCodeInfo)
      }
      try {
        return excessKurtosis(vector)
      }
      catch (error) {
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: 1,
  },
  'vec:sample-kurtosis': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < 4) {
        throw new LitsError('Kurtosis requires at least four values', sourceCodeInfo)
      }
      try {
        return sampleKurtosis(vector)
      }
      catch (error) {
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: 1,
  },
  'vec:sample-excess-kurtosis': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      if (vector.length < 4) {
        throw new LitsError('Kurtosis requires at least four values', sourceCodeInfo)
      }
      try {
        return sampleExcessKurtosis(vector)
      }
      catch (error) {
        if (error instanceof Error) {
          throw new LitsError(error.message, sourceCodeInfo)
        }
        throw error
      }
    },
    paramCount: 1,
  },
  'vec:geometric-mean': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return Math.exp(vector.reduce((acc, val) => acc + Math.log(val), 0) / vector.length)
    },
    paramCount: 1,
  },
  'vec:harmonic-mean': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return vector.length / vector.reduce((acc, val) => acc + 1 / val, 0)
    },
    paramCount: 1,
  },
  'vec:rms': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return Math.sqrt(vector.reduce((acc, val) => acc + val ** 2, 0) / vector.length)
    },
    paramCount: 1,
  },
  'vec:mad': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return calcMad(vector)
    },
    paramCount: 1,
  },

  'vec:medad': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return calcMedad(vector)
    },
    paramCount: 1,
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
  'vec:moving-median': {
    evaluate: ([vector, windowSize], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })

      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const median = vector.slice(i, i + windowSize).sort((a, b) => a - b)[Math.floor(windowSize / 2)]!
        result.push(median)
      }
      return result
    },
    paramCount: 2,
  },
  'vec:moving-std': {
    evaluate: ([vector, windowSize], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })

      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const stdDev = calcStdDev(vector.slice(i, i + windowSize))
        result.push(stdDev)
      }
      return result
    },
    paramCount: 2,
  },
  'vec:moving-variance': {
    evaluate: ([vector, windowSize], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })
      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        result.push(calcVariance(vector.slice(i, i + windowSize)))
      }
      return result
    },
    paramCount: 2,
  },
  'vec:moving-rms': {
    evaluate: ([vector, windowSize], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })
      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const mean = calcMean(vector.slice(i, i + windowSize))
        const rms = Math.sqrt(vector.slice(i, i + windowSize).reduce((acc, val) => acc + (val - mean) ** 2, 0) / windowSize)
        result.push(rms)
      }
      return result
    },
    paramCount: 2,
  },
  'vec:moving-percentile': {
    evaluate: ([vector, windowSize, percentile], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(percentile, sourceCodeInfo, { finite: true })

      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const sorted = vector.slice(i, i + windowSize).sort((a, b) => a - b)
        const index = Math.floor(percentile * (sorted.length - 1))
        result.push(sorted[index]!)
      }
      return result
    },
    paramCount: 3,
  },
  'vec:moving-quantile': {
    evaluate: ([vector, windowSize, quantile], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })
      assertNumber(quantile, sourceCodeInfo, { finite: true })

      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const sorted = vector.slice(i, i + windowSize).sort((a, b) => a - b)
        const index = Math.floor(quantile * (sorted.length - 1))
        result.push(sorted[index]!)
      }
      return result
    },
    paramCount: 3,
  },
  'vec:entropy': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      return calculateEntropy(vector)
    },
    paramCount: 1,
  },
  'vec:gini-coefficient': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      const sorted = [...vector].sort((a, b) => a - b)
      const n = sorted.length
      const sum = sorted.reduce((acc, val) => acc + val, 0)
      const gini = (2 * sorted.reduce((acc, val, i) => acc + (i + 1) * val, 0)) / (n * sum) - (n + 1) / n
      return gini
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
      if (weights !== null) {
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
  'vec:arithmetic-sum': {
    evaluate: ([start, step, length], sourceCodeInfo): number => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      return (length / 2) * (2 * start + (length - 1) * step)
    },
    paramCount: 3,
  },
  'vec:winsorize': {
    evaluate: ([vector, lowerPercentile, upperPercentile], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(lowerPercentile, sourceCodeInfo, { finite: true })
      assertNumber(upperPercentile, sourceCodeInfo, { finite: true })

      if (vector.length === 0)
        return []

      if (lowerPercentile < 0 || lowerPercentile > 0.5 || upperPercentile < 0 || upperPercentile > 0.5) {
        throw new LitsError('Percentiles must be between 0 and 0.5', sourceCodeInfo)
      }

      const sorted = [...vector].sort((a, b) => a - b)

      const lowerIndex = Math.max(0, Math.floor(lowerPercentile * vector.length))
      const upperIndex = Math.min(vector.length - 1, Math.floor((1 - upperPercentile) * vector.length))

      const lowerBound = sorted[lowerIndex]!
      const upperBound = sorted[upperIndex]!

      return vector.map(val => Math.max(lowerBound, Math.min(val, upperBound)))
    },
    paramCount: 3,
  },
  'vec:mse': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return vectorA.reduce((acc, val, i) => acc + (val - vectorB[i]!) ** 2, 0) / vectorA.length
    },
    paramCount: 2,
  },
  'vec:mae': {
    evaluate: ([vectorA, vectorB], sourceCodeInfo): number => {
      assertVector(vectorA, sourceCodeInfo)
      assertVector(vectorB, sourceCodeInfo)
      if (vectorA.length !== vectorB.length) {
        throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
      }
      return vectorA.reduce((acc, val, i) => acc + Math.abs(val - vectorB[i]!), 0) / vectorA.length
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
