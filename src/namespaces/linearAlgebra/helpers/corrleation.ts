import { calcMean } from '../../vector/calcMean'
import { calcStdDev } from '../../vector/calcStdDev'
import { calcCovariance } from './covariance'

/**
 * Calculate Pearson correlation between two segments
 */
export function calcCorrelation(segmentA: number[], segmentB: number[]): number {
  const meanA = calcMean(segmentA)
  const meanB = calcMean(segmentB)

  const stdA = calcStdDev(segmentA, meanA)
  const stdB = calcStdDev(segmentB, meanB)

  // Handle zero variance
  if (stdA === 0 || stdB === 0) {
    // If both have zero variance and are the same constant, they're perfectly correlated
    if (stdA === 0 && stdB === 0 && meanA === meanB) {
      return 1
    }
    // Otherwise, no meaningful correlation can be established
    return 0
  }

  const covariance = calcCovariance(segmentA, segmentB)
  return covariance / (stdA * stdB)
}

/**
 * Extract overlapping segments from two vectors based on lag
 */
export function extractOverlappingSegments(
  vectorA: number[],
  vectorB: number[],
  lag: number,
): [number[], number[]] {
  const absLag = Math.abs(lag)
  const overlapLength = vectorA.length - absLag

  let segmentA = []
  let segmentB = []

  if (lag >= 0) {
    segmentA = vectorA.slice(0, overlapLength)
    segmentB = vectorB.slice(lag, lag + overlapLength)
  }
  else {
    segmentA = vectorA.slice(absLag)
    segmentB = vectorB.slice(0, overlapLength)
  }

  return [segmentA, segmentB]
}
