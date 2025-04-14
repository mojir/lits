import { calcMean } from '../../vector/calcMean'

/**
 * Calculate covariance between two segments
 */
export function calcCovariance(segmentA: number[], segmentB: number[]): number {
  const meanA = calcMean(segmentA)
  const meanB = calcMean(segmentB)

  let sum = 0
  for (let i = 0; i < segmentA.length; i++) {
    sum += (segmentA[i]! - meanA) * (segmentB[i]! - meanB)
  }

  return sum / segmentA.length
}
