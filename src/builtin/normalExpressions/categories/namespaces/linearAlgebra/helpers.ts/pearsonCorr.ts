import { calcMean } from '../../vector/calcMean'

export function pearsonCorr(vectorA: number[], vectorB: number[]): number {
  const meanA = calcMean(vectorA)
  const meanB = calcMean(vectorB)

  let sumNumerator = 0
  let sumSquareA = 0
  let sumSquareB = 0

  for (let i = 0; i < vectorA.length; i++) {
    const diffA = vectorA[i]! - meanA
    const diffB = vectorB[i]! - meanB

    sumNumerator += diffA * diffB
    sumSquareA += diffA * diffA
    sumSquareB += diffB * diffB
  }

  // Check if either vector has zero variance
  if (sumSquareA === 0 || sumSquareB === 0) {
    throw new Error('Cannot calculate Pearson correlation coefficient: one of the vectors has zero variance.')
  }

  return sumNumerator / (Math.sqrt(sumSquareA) * Math.sqrt(sumSquareB))
}
