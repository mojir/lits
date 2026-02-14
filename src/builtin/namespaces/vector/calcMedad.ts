import { calcMedian } from './calcMedian'

export function calcMedad(vector: number[]): number {
  const median = calcMedian(vector)
  // Calculate absolute deviations from the median
  const absoluteDeviations = vector.map(val => Math.abs(val - median))

  // Calculate the median of the absolute deviations
  const medianOfDeviations = calcMedian(absoluteDeviations)
  const scaleFactor = 1.4826 // Scale factor for robust scaling

  return medianOfDeviations * scaleFactor
}
