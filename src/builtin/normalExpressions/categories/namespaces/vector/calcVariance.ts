import { calcMean } from './calcMean'

export function calcVariance(vector: number[]): number {
  if (vector.length === 0) {
    return 0
  }

  const mean = calcMean(vector)
  const variance
    = vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / vector.length

  return variance
}
