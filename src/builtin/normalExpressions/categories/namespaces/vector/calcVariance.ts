import { calcMean } from './calcMean'

export function calcVariance(vector: number[]): number {
  const mean = calcMean(vector)
  return vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / vector.length
}

export function calcSampleVariance(vector: number[]): number {
  const mean = calcMean(vector)
  return vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / (vector.length - 1)
}
