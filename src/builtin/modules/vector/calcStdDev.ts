import { calcVariance } from './calcVariance'

export function calcStdDev(vector: number[], optionalMean?: number): number {
  const variance = calcVariance(vector, optionalMean)
  return Math.sqrt(variance)
}

export function calcSampleStdDev(vector: number[], optionalMean?: number): number {
  const variance = calcVariance(vector, optionalMean)
  return Math.sqrt(variance * (vector.length / (vector.length - 1)))
}
