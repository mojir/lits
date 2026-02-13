import { calcMean } from '../calcMean'
import { calcStdDev } from '../calcStdDev'
import type { ReductionFunctionDefinition } from '.'

function skewness(vector: number[]): number {
  const mean = calcMean(vector)
  const stdDev = calcStdDev(vector)
  if (stdDev === 0) {
    throw new Error('Standard deviation is zero, skewness is undefined')
  }
  return vector.reduce((acc, val) => acc + ((val - mean) ** 3), 0) / (vector.length * stdDev ** 3)
}

function sampleSkewness(vector: number[]): number {
  const n = vector.length

  // Calculate the mean
  const mean = vector.reduce((acc, val) => acc + val, 0) / n

  // Calculate sum of squared differences and sum of cubed differences
  let sumSquaredDiffs = 0
  let sumCubedDiffs = 0

  for (const val of vector) {
    const diff = val - mean
    sumSquaredDiffs += diff * diff
    sumCubedDiffs += diff * diff * diff
  }

  // Calculate sample standard deviation (using n-1)
  const sampleVariance = sumSquaredDiffs / (n - 1)
  const sampleStdDev = Math.sqrt(sampleVariance)

  // If standard deviation is 0, skewness is undefined
  if (sampleStdDev === 0) {
    throw new Error('Cannot calculate sample skewness when standard deviation is 0')
  }

  // Calculate sample skewness with Fisher's adjustment
  return (n / ((n - 1) * (n - 2))) * sumCubedDiffs / sampleStdDev ** 3
}

export const skewnessReductionFunction: ReductionFunctionDefinition<'skewness'> = {
  skewness: vector => skewness(vector),
  minLength: 3,
}

export const sampleSkewnessReductionFunction: ReductionFunctionDefinition<'sample-skewness'> = {
  'sample-skewness': vector => sampleSkewness(vector),
  'minLength': 3,
}
