import { LitsError } from '../../../../../errors'
import { assertNonEmptyVector, assertVector, isVector } from '../../../../../typeGuards/annotatedArrays'
import { assertLitsFunction } from '../../../../../typeGuards/litsFunction'
import { assertNumber } from '../../../../../typeGuards/number'
import type { BuiltinNormalExpressions } from '../../../../interface'
import { bincount } from './bincount'
import { calcMean } from './calcMean'
import { calcStdDev } from './calcStdDev'
import { calcVariance } from './calcVariance'
import { calculateEntropy } from './entropy'
import { mode } from './mode'

export const vectorNormalExpression: BuiltinNormalExpressions = {
  'v:vector?': {
    evaluate: ([vector]): boolean => isVector(vector),
    paramCount: 1,
  },
  'v:sorted?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val >= vector[i - 1]!)
    },
    paramCount: 1,
  },
  'v:monotonic?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val >= vector[i - 1]!)
        || vector.every((val, i) => i === 0 || val <= vector[i - 1]!)
    },
    paramCount: 1,
  },
  'v:strictly-monotonic?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val > vector[i - 1]!)
        || vector.every((val, i) => i === 0 || val < vector[i - 1]!)
    },
    paramCount: 1,
  },
  'v:increasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val >= vector[i - 1]!)
    },
    paramCount: 1,
  },
  'v:decreasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val <= vector[i - 1]!)
    },
    paramCount: 1,
  },
  'v:strictly-increasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val > vector[i - 1]!)
    },
    paramCount: 1,
  },
  'v:strictly-decreasing?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)
      return vector.every((val, i) => i === 0 || val < vector[i - 1]!)
    },
    paramCount: 1,
  },
  'v:+': {
    evaluate: (params, sourceCodeInfo): number[] => {
      const firstParam = params[0]!
      assertVector(firstParam, sourceCodeInfo)
      const restParams = params.slice(1)
      for (const param of restParams) {
        assertVector(param, sourceCodeInfo)
        if (firstParam.length !== param.length) {
          throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
        }
      }
      const rest = restParams as number[][]
      return rest.reduce((acc, vector) => acc.map((val, i) => val + vector[i]!), firstParam)
    },
    paramCount: { min: 1 },
  },
  'v:-': {
    evaluate: (params, sourceCodeInfo): number[] => {
      const firstParam = params[0]!
      assertVector(firstParam, sourceCodeInfo)
      const restParams = params.slice(1)
      for (const param of restParams) {
        assertVector(param, sourceCodeInfo)
        if (firstParam.length !== param.length) {
          throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
        }
      }
      if (restParams.length === 0) {
        return firstParam.map(val => -val)
      }
      const rest = restParams as number[][]
      return rest.reduce((acc, vector) => acc.map((val, i) => val - vector[i]!), firstParam)
    },
    paramCount: { min: 1 },
  },
  'v:*': {
    evaluate: (params, sourceCodeInfo): number[] => {
      const firstParam = params[0]!
      assertVector(firstParam, sourceCodeInfo)
      const restParams = params.slice(1)
      for (const param of restParams) {
        assertVector(param, sourceCodeInfo)
        if (firstParam.length !== param.length) {
          throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
        }
      }
      const rest = restParams as number[][]
      return rest.reduce((acc, vector) => acc.map((val, i) => val * vector[i]!), firstParam)
    },
    paramCount: { min: 1 },
  },
  'v:/': {
    evaluate: (params, sourceCodeInfo): number[] => {
      const firstParam = params[0]!
      assertVector(firstParam, sourceCodeInfo)
      const restParams = params.slice(1)
      for (const param of restParams) {
        assertVector(param, sourceCodeInfo)
        if (firstParam.length !== param.length) {
          throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
        }
      }
      if (restParams.length === 0) {
        return firstParam.map(val => 1 / val)
      }
      const rest = restParams as number[][]
      return rest.reduce((acc, vector) => acc.map((val, i) => val / vector[i]!), firstParam)
    },
    paramCount: { min: 1 },
  },
  'v:^': {
    evaluate: ([a, b], sourceCodeInfo): number[] => {
      if (!isVector(a) && !isVector(b)) {
        throw new LitsError('At least one parameter must be a vector', sourceCodeInfo)
      }
      if (!isVector(a)) {
        assertVector(b, sourceCodeInfo)
        assertNumber(a, sourceCodeInfo, { finite: true })
      }
      if (!isVector(b)) {
        assertVector(a, sourceCodeInfo)
        assertNumber(b, sourceCodeInfo, { finite: true })
      }
      const length = (isVector(a) ? a : (b as number[])).length

      const [vector1, vector2] = [a, b].map<number[]>((operand) => {
        if (isVector(operand)) {
          if (operand.length !== length) {
            throw new LitsError('Vectors must be of the same length', sourceCodeInfo)
          }
          return operand
        }
        return Array.from({ length }, () => operand)
      })

      return vector1!.map((val, i) => {
        const exponent = vector2![i]!
        if (exponent < 0 && val === 0) {
          throw new LitsError('Cannot raise zero to a negative exponent', sourceCodeInfo)
        }
        return val ** exponent
      })
    },
    paramCount: 2,
  },
  'v:scale': {
    evaluate: ([vector, scalar], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(scalar, sourceCodeInfo, { finite: true })

      return vector.map(val => val * scalar)
    },
    paramCount: 2,
  },
  'v:abs': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return vector.map(val => Math.abs(val))
    },
    paramCount: 1,
  },
  'v:sum': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => acc + val, 0)
    },
    paramCount: 1,
  },
  'v:product': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => acc * val, 1)
    },
    paramCount: 1,
  },
  'v:mean': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return calcMean(vector)
    },
    paramCount: 1,
  },
  'v:median': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      const sorted = [...vector].sort((a, b) => a - b)
      const mid = Math.floor(sorted.length / 2)

      return sorted.length % 2 === 0
        ? (sorted[mid - 1]! + sorted[mid]!) / 2
        : sorted[mid]!
    },
    paramCount: 1,
  },
  'v:mode': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      return mode(vector)
    },
    paramCount: 1,
  },
  'v:variance': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      const mean = calcMean(vector)
      return vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / vector.length
    },
    paramCount: 1,
  },
  'v:std-dev': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return Math.sqrt(vector.reduce((acc, val) => acc + val, 0) / vector.length)
    },
    paramCount: 1,
  },
  'v:min': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      return vector.reduce((acc, val) => (val < vector[acc]! ? val : acc), vector[0]!)
    },
    paramCount: 1,
  },
  'v:max': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)
      return vector.reduce((acc, val) => (val > vector[acc]! ? val : acc), vector[0]!)
    },
    paramCount: 1,
  },
  'v:min-index': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val, i) => (val < vector[acc]! ? i : acc), 0)
    },
    paramCount: 1,
  },
  'v:max-index': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val, i) => (val > vector[acc]! ? i : acc), 0)
    },
    paramCount: 1,
  },
  'v:sort-indices': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return [...vector.keys()].sort((a, b) => vector[a]! - vector[b]!)
    },
    paramCount: 1,
  },
  'v:count-values': {
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
  'v:linspace': {
    evaluate: ([start, end, numPoints], sourceCodeInfo): number[] => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(end, sourceCodeInfo, { finite: true })
      assertNumber(numPoints, sourceCodeInfo, { integer: true, positive: true })

      const step = (end - start) / (numPoints - 1)
      return Array.from({ length: numPoints }, (_, i) => start + i * step)
    },
    paramCount: 3,
  },
  'v:ones': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      return Array.from({ length }, () => 1)
    },
    paramCount: 1,
  },
  'v:zeros': {
    evaluate: ([length], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      return Array.from({ length }, () => 0)
    },
    paramCount: 1,
  },
  'v:fill': {
    evaluate: ([length, value], sourceCodeInfo): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      return Array.from({ length }, () => value) as number[]
    },
    paramCount: 2,
  },
  'v:generate': {
    evaluate: ([length, generator], sourceCodeInfo, contextStack, { executeFunction }): number[] => {
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })
      assertLitsFunction(generator, sourceCodeInfo)

      return Array.from({ length }, (_, i) => {
        const value = executeFunction(generator, [i], contextStack, sourceCodeInfo)
        assertNumber(value, sourceCodeInfo, { finite: true })
        return value
      })
    },
    paramCount: 2,
  },
  'v:cumsum': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => {
        const last = acc[acc.length - 1] || 0
        acc.push(last + val)
        return acc
      }, [] as number[])
    },
    paramCount: 1,
  },
  'v:cumprod': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      return vector.reduce((acc, val) => {
        const last = acc[acc.length - 1] || 1
        acc.push(last * val)
        return acc
      }, [] as number[])
    },
    paramCount: 1,
  },
  'v:quartiles': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      const sorted = [...vector].sort((a, b) => a - b)
      const q1 = sorted[Math.floor((sorted.length / 4))]!
      const q2 = sorted[Math.floor((sorted.length / 2))]!
      const q3 = sorted[Math.floor((3 * sorted.length) / 4)]!

      return [q1, q2, q3]
    },
    paramCount: 1,
  },
  'v:iqr': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      const sorted = [...vector].sort((a, b) => a - b)
      const q1 = sorted[Math.floor((sorted.length / 4))]!
      const q3 = sorted[Math.floor((3 * sorted.length) / 4)]!

      return q3 - q1
    },
    paramCount: 1,
  },
  'v:percentile': {
    evaluate: ([vector, percentile], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(percentile, sourceCodeInfo, { finite: true })

      const sorted = [...vector].sort((a, b) => a - b)
      const index = Math.floor((percentile / 100) * (sorted.length - 1))
      return sorted[index]!
    },
    paramCount: 2,
  },
  'v:quantile': {
    evaluate: ([vector, quantile], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(quantile, sourceCodeInfo, { finite: true })

      const sorted = [...vector].sort((a, b) => a - b)
      const index = Math.floor(quantile * (sorted.length - 1))
      return sorted[index]!
    },
    paramCount: 2,
  },
  'v:range': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertNonEmptyVector(vector, sourceCodeInfo)

      const min = vector.reduce((acc, val) => (val < vector[acc]! ? val : acc), vector[0]!)
      const max = vector.reduce((acc, val) => (val > vector[acc]! ? val : acc), vector[0]!)

      return max - min
    },
    paramCount: 1,
  },
  'v:skewness': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      const mean = calcMean(vector)
      const stdDev = calcStdDev(vector)
      return vector.reduce((acc, val) => acc + ((val - mean) ** 3), 0) / (vector.length * stdDev ** 3)
    },
    paramCount: 1,
  },
  'v:kurtosis': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      const mean = calcMean(vector)
      const stdDev = calcStdDev(vector)
      return vector.reduce((acc, val) => acc + ((val - mean) ** 4), 0) / (vector.length * stdDev ** 4)
    },
    paramCount: 1,
  },
  'v:geometric-mean': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return Math.exp(vector.reduce((acc, val) => acc + Math.log(val), 0) / vector.length)
    },
    paramCount: 1,
  },
  'v:harmonic-mean': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return vector.length / vector.reduce((acc, val) => acc + 1 / val, 0)
    },
    paramCount: 1,
  },
  'v:rms': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)

      return Math.sqrt(vector.reduce((acc, val) => acc + val ** 2, 0) / vector.length)
    },
    paramCount: 1,
  },
  'v:z-score': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      const mean = calcMean(vector)
      const stdDev = calcStdDev(vector)

      return vector.map(val => (val - mean) / stdDev)
    },
    paramCount: 1,
  },
  'v:normalize-minmax': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      const min = vector.reduce((acc, val) => (val < vector[acc]! ? val : acc), vector[0]!)
      const max = vector.reduce((acc, val) => (val > vector[acc]! ? val : acc), vector[0]!)

      return vector.map(val => (val - min) / (max - min))
    },
    paramCount: 1,
  },
  'v:normalize-robust': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      const median = vector.reduce((acc, val) => acc + val, 0) / vector.length
      const iqr = vector.reduce((acc, val) => acc + Math.abs(val - median), 0) / vector.length

      return vector.map(val => (val - median) / iqr)
    },
    paramCount: 1,
  },
  'v:histogram': {
    evaluate: ([vector, bins], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(bins, sourceCodeInfo, { integer: true, positive: true })

      const min = vector.reduce((acc, val) => (val < vector[acc]! ? val : acc), vector[0]!)
      const max = vector.reduce((acc, val) => (val > vector[acc]! ? val : acc), vector[0]!)

      const binSize = (max - min) / bins
      const histogram = Array.from({ length: bins }, () => 0)

      for (const value of vector) {
        const binIndex = Math.floor((value - min) / binSize)
        if (binIndex >= 0 && binIndex < bins) {
          histogram[binIndex]! += 1
        }
      }

      return histogram
    },
    paramCount: 2,
  },
  'v:cdf': {
    evaluate: ([vector, value], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(value, sourceCodeInfo, { finite: true })

      const sorted = [...vector].sort((a, b) => a - b)
      const index = sorted.findIndex(val => val > value)

      return index === -1 ? 1 : index / sorted.length
    },
    paramCount: 2,
  },
  'v:ecdf': {
    evaluate: ([vector, value], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(value, sourceCodeInfo, { finite: true })

      const sorted = [...vector].sort((a, b) => a - b)
      const index = sorted.findIndex(val => val >= value)

      return index === -1 ? 1 : (index + 1) / sorted.length
    },
    paramCount: 2,
  },
  'v:no-extreme-eutliers?': {
    evaluate: ([vector], sourceCodeInfo): boolean => {
      assertVector(vector, sourceCodeInfo)

      const mean = calcMean(vector)
      const stdDev = calcStdDev(vector)

      return vector.every(val => Math.abs((val - mean) / stdDev) < 3)
    },
    paramCount: 1,
  },
  'v:outliers': {
    evaluate: ([vector], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)

      const mean = calcMean(vector)
      const stdDev = calcStdDev(vector)

      return vector.filter(val => Math.abs((val - mean) / stdDev) > 3)
    },
    paramCount: 1,
  },
  'v:moving-average': {
    evaluate: ([vector, windowSize], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })

      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const sum = vector.slice(i, i + windowSize).reduce((acc, val) => acc + val, 0)
        result.push(sum / windowSize)
      }
      return result
    },
    paramCount: 2,
  },
  'v:moving-median': {
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
  'v:moving-std': {
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
  'v:moving-sum': {
    evaluate: ([vector, windowSize], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })
      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const sum = vector.slice(i, i + windowSize).reduce((acc, val) => acc + val, 0)
        result.push(sum)
      }
      return result
    },
    paramCount: 2,
  },
  'v:moving-product': {
    evaluate: ([vector, windowSize], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })
      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const product = vector.slice(i, i + windowSize).reduce((acc, val) => acc * val, 1)
        result.push(product)
      }
      return result
    },
    paramCount: 2,
  },
  'v:moving-min': {
    evaluate: ([vector, windowSize], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })
      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const min = vector.slice(i, i + windowSize).reduce((acc, val) => (val < acc ? val : acc), vector[i]!)
        result.push(min)
      }
      return result
    },
    paramCount: 2,
  },
  'v:moving-max': {
    evaluate: ([vector, windowSize], sourceCodeInfo): number[] => {
      assertVector(vector, sourceCodeInfo)
      assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true })
      const result = []
      for (let i = 0; i < vector.length - windowSize + 1; i += 1) {
        const max = vector.slice(i, i + windowSize).reduce((acc, val) => (val > acc ? val : acc), vector[i]!)
        result.push(max)
      }
      return result
    },
    paramCount: 2,
  },
  'moving-variance': {
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
  'moving-rms': {
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
  'v:moving-percentile': {
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
  'v:moving-quantile': {
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
  'v:entropy': {
    evaluate: ([vector], sourceCodeInfo): number => {
      assertVector(vector, sourceCodeInfo)
      return calculateEntropy(vector)
    },
    paramCount: 1,
  },
  'v:gini-coefficient': {
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
  'v:bincount': {
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
  'v:arithmetic-sum': {
    evaluate: ([start, step, length], sourceCodeInfo): number => {
      assertNumber(start, sourceCodeInfo, { finite: true })
      assertNumber(step, sourceCodeInfo, { finite: true })
      assertNumber(length, sourceCodeInfo, { integer: true, positive: true })

      return (length / 2) * (2 * start + (length - 1) * step)
    },
    paramCount: 3,
  },
  'v:winsorize': {
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
  'v:mse': {
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
  'v:mae': {
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
