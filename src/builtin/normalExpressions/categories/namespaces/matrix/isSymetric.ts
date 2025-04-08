import { isSquare } from './isSquare'

/**
 * Checks if a given matrix is symmetric.
 * A matrix is symmetric if it is square and its transpose is equal to itself.
 *
 * @param matrix - A 2D array representing the matrix.
 * @returns `true` if the matrix is symmetric, otherwise `false`.
 */
export function isSymetric(matrix: number[][]): boolean {
  const rows = matrix.length

  // Check if the matrix is square
  if (!isSquare(matrix)) {
    return false
  }

  // Check symmetry
  for (let i = 0; i < rows; i += 1) {
    for (let j = 0; j < i; j += 1) {
      if (matrix[i]![j]! !== matrix[j]![i]!) {
        return false
      }
    }
  }

  return true
}
