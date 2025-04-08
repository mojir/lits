import { calcVariance } from './calcVariance'

export function calcStdDev(vector: number[]): number {
  if (vector.length === 0) {
    return 0
  }

  const variance = calcVariance(vector)
  return Math.sqrt(variance)
}
