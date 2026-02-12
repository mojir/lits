import { calcMean } from '../calcMean'
import { calcStdDev } from '../calcStdDev'

import type { ReductionFunctionDefinition } from '.'

function kurtosis(vector: number[]): number {
  const mean = calcMean(vector)
  const stdDev = calcStdDev(vector)
  if (stdDev === 0) {
    throw new Error('Standard deviation is zero, kurtosis is undefined')
  }
  return vector.reduce((acc, val) => acc + ((val - mean) ** 4), 0) / (vector.length * stdDev ** 4)
}

function excessKurtosis(vector: number[]): number {
  return kurtosis(vector) - 3
}

/**
 * Calculates the sample kurtosis (bias-corrected)
 * @param vector Array of numeric values
 * @returns The sample kurtosis
 * @throws Error if sample size is less than 4 or if variance is zero
 */
function sampleKurtosis(vector: number[]): number {
  const n = vector.length

  const mean = vector.reduce((sum, val) => sum + val, 0) / n
  const sumSquaredDeviations = vector.reduce((sum, val) => sum + (val - mean) ** 2, 0)
  const variance = sumSquaredDeviations / (n - 1)

  if (variance === 0) {
    throw new Error('Variance is zero, kurtosis is undefined')
  }

  const fourthMomentSum = vector.reduce((sum, val) => sum + (val - mean) ** 4, 0)

  // Correct formula for sample kurtosis
  return (n * (n + 1) * fourthMomentSum) / ((n - 1) * (n - 2) * (n - 3) * variance ** 2)
}
/**
 * Calculates the sample excess kurtosis (bias-corrected)
 * @param vector Array of numeric values
 * @returns The sample excess kurtosis
 * @throws Error if sample size is less than 4 or if variance is zero
 */
function sampleExcessKurtosis(vector: number[]): number {
  const n = vector.length

  const mean = vector.reduce((sum, val) => sum + val, 0) / n
  const sumSquaredDeviations = vector.reduce((sum, val) => sum + (val - mean) ** 2, 0)
  const variance = sumSquaredDeviations / (n - 1)

  if (variance === 0) {
    throw new Error('Variance is zero, kurtosis is undefined')
  }

  const fourthMomentSum = vector.reduce((sum, val) => sum + (val - mean) ** 4, 0)
  const rawKurtosis = (n * (n + 1) * fourthMomentSum) / ((n - 1) * (n - 2) * (n - 3) * variance ** 2)

  // Compute excess kurtosis by subtracting 3 times the bias correction factor
  return rawKurtosis - (3 * (n - 1) * (n - 1)) / ((n - 2) * (n - 3))
}

export const kurtosisReductionFunction: ReductionFunctionDefinition<'kurtosis'> = {
  'kurtosis': vector => kurtosis(vector),
  'minLength': 4,
}

export const eccessKurtosisReductionFunction: ReductionFunctionDefinition<'excess-kurtosis'> = {
  'excess-kurtosis': vector => excessKurtosis(vector),
  'minLength': 4,
}

export const sampleKurtosisReductionFunction: ReductionFunctionDefinition<'sample-kurtosis'> = {
  'sample-kurtosis': vector => sampleKurtosis(vector),
  'minLength': 4,
}

export const sampleExcessKurtosisReductionFunction: ReductionFunctionDefinition<'sample-excess-kurtosis'> = {
  'sample-excess-kurtosis': vector => sampleExcessKurtosis(vector),
  'minLength': 4,
}
