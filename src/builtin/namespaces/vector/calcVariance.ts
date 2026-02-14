import { calcMean } from './calcMean'

export function calcVariance(vector: number[], optionalMean?: number): number {
  const mean = optionalMean ?? calcMean(vector)
  return vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / vector.length
}

export function calcSampleVariance(vector: number[], optionalMean?: number): number {
  const mean = optionalMean ?? calcMean(vector)
  return vector.reduce((acc, val) => acc + (val - mean) ** 2, 0) / (vector.length - 1)
}
