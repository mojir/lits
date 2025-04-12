import { calcVariance } from './calcVariance'

export function calcStdDev(vector: number[]): number {
  const variance = calcVariance(vector)
  return Math.sqrt(variance)
}

export function calcSampleStdDev(vector: number[]): number {
  const variance = calcVariance(vector)
  return Math.sqrt(variance * (vector.length / (vector.length - 1)))
}
