import { isSquare } from './isSquare'

/**
 * Determines whether a given matrix is triangular.
 *
 * A triangular matrix is a square matrix where all elements
 * below or above the main diagonal are zero. This function
 * checks if the matrix is square and symmetric.
 *
 * @param matrix - A two-dimensional array of numbers representing the matrix.
 * @returns `true` if the matrix is triangular, otherwise `false`.
 */
export function isTriangular(matrix: number[][]): boolean {
  if (!isSquare(matrix)) {
    return false
  }

  const rows = matrix.length

  let isUpperTriangular = true
  let isLowerTriangular = true

  for (let i = 0; i < rows; i++) {
    for (let j = 0; j < rows; j++) {
      if (i > j && matrix[i]![j] !== 0) {
        isUpperTriangular = false
        if (!isLowerTriangular) {
          return false
        }
      }
      if (i < j && matrix[i]![j] !== 0) {
        isLowerTriangular = false
        if (!isUpperTriangular) {
          return false
        }
      }
    }
  }

  return isUpperTriangular || isLowerTriangular
}

export function isTriangularUpper(matrix: number[][]): boolean {
  if (!isSquare(matrix)) {
    return false
  }
  const rows = matrix.length

  for (let i = 0; i < rows; i++) {
    for (let j = 0; j < i; j++) {
      if (matrix[i]![j] !== 0) {
        return false
      }
    }
  }

  return true
}

export function isTriangularLower(matrix: number[][]): boolean {
  if (!isSquare(matrix)) {
    return false
  }
  const rows = matrix.length

  // Check if the matrix is square
  if (!matrix.every(row => row.length === rows)) {
    return false
  }

  for (let i = 0; i < rows; i++) {
    for (let j = i + 1; j < rows; j++) {
      if (matrix[i]![j] !== 0) {
        return false
      }
    }
  }

  return true
}
