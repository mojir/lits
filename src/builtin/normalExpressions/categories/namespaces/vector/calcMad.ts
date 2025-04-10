import { calcMedian } from './calcMedian'

export function calcMad(vector: number[]): number {
  const median = calcMedian(vector)

  // Calculate mean absolute deviation
  return vector.reduce((acc, val) => acc + Math.abs(val - median), 0) / vector.length
}
