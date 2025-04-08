import { isSquare } from './isSquare'

/**
 * Checks if a given matrix is diagonal.
 *
 * A matrix is considered diagonal if it is square (i.e., the number of rows equals the number of columns)
 * and all elements outside the main diagonal are zero.
 *
 * @param matrix - A two-dimensional array of numbers representing the matrix to check.
 * @returns `true` if the matrix is diagonal, otherwise `false`.
 */
export function isDiagonal(matrix: number[][]): boolean {
  if (!isSquare(matrix)) {
    return false
  }
  const rows = matrix.length

  for (let i = 0; i < rows; i += 1) {
    for (let j = 0; j < rows; j += 1) {
      if (i !== j && matrix[i]![j] !== 0) {
        return false
      }
    }
  }

  return true
}
