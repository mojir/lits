import { approxZero } from '../../../../../../utils'

/**
 * Calculates the determinant of a matrix using Gaussian Elimination
 * @param matrix A square matrix represented as a 2D array
 * @returns The determinant of the matrix
 */
export function determinant(matrix: number[][]): number {
  // First, make a deep copy of the matrix to avoid modifying the original
  const n = matrix.length
  const A: number[][] = []

  for (let i = 0; i < n; i++) {
    A[i] = [...matrix[i]!]
  }

  // Handle special cases for small matrices
  if (n === 1) {
    return A[0]![0]!
  }

  if (n === 2) {
    return A[0]![0]! * A[1]![1]! - A[0]![1]! * A[1]![0]!
  }

  // For larger matrices, use Gaussian elimination
  let sign = 1 // Track sign changes from row swaps

  // Perform Gaussian elimination to get an upper triangular matrix
  for (let i = 0; i < n - 1; i += 1) {
    // Find pivot (maximum element in current column)
    let maxRow = i

    for (let j = i + 1; j < n; j += 1) {
      if (Math.abs(A[j]![i]!) > Math.abs(A[maxRow]![i]!)) {
        maxRow = j
      }
    }

    // If the pivot is zero, the determinant is zero
    if (approxZero(A[maxRow]![i]!)) {
      return 0
    }

    // Swap rows if necessary
    if (maxRow !== i) {
      [A[i], A[maxRow]] = [A[maxRow]!, A[i]!] // ES6 array destructuring for swap
      sign = -sign // Each row swap changes the sign
    }

    // Eliminate entries below the pivot
    for (let j = i + 1; j < n; j += 1) {
      const factor = A[j]![i]! / A[i]![i]!

      // Subtract (factor * pivot row) from current row
      for (let k = i; k < n; k++) {
        A[j]![k]! -= factor * A[i]![k]!
      }
    }
  }

  // Calculate determinant as the product of diagonal elements
  let det = sign
  for (let i = 0; i < n; i++) {
    det *= A[i]![i]!
  }

  return det
}
