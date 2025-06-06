/**
 * Calculate the inverse of a matrix using the adjugate method
 * @param matrix The input matrix
 * @returns The inverse matrix or null if the matrix is not invertible
 */

import { approxZero } from '../../../../../../utils'
import { adjugate } from './adjugate'
import { determinant } from './determinant'

export function inverse(matrix: number[][]): number[][] | null {
  const n = matrix.length

  // Special case for 1x1 matrix - handle it directly
  if (n === 1) {
    const element = matrix[0]![0]!
    if (approxZero(element)) {
      return null // Not invertible
    }
    return [[1 / element]]
  }

  // Calculate determinant
  const det = determinant(matrix)

  // Check if matrix is invertible
  if (approxZero(det)) {
    return null // Matrix is not invertible
  }

  // Get the adjugate matrix
  const adj = adjugate(matrix)

  // Calculate the inverse: inverse = adjugate / determinant
  const inverseMatrix: number[][] = []
  for (let i = 0; i < n; i++) {
    inverseMatrix[i] = []
    for (let j = 0; j < n; j++) {
      inverseMatrix[i]![j] = adj[i]![j]! / det
    }
  }

  return inverseMatrix
}
