/**
 * Checks if a matrix is banded with the given lower and upper bandwidth.
 * A matrix is banded if all non-zero elements are within 'lower' diagonals
 * below the main diagonal and 'upper' diagonals above the main diagonal.
 *
 * @param matrix - The matrix to check, represented as a 2D array of numbers
 * @param lower - Number of non-zero diagonals below the main diagonal
 * @param upper - Number of non-zero diagonals above the main diagonal
 * @returns true if the matrix is banded with the given parameters, false otherwise
 */
export function isBanded(matrix: number[][], lower: number, upper: number): boolean {
  const rows = matrix.length
  const cols = matrix[0]!.length

  // Check each element in the matrix
  for (let i = 0; i < rows; i++) {
    for (let j = 0; j < cols; j++) {
      // If we find a non-zero element outside the band, return false
      if (matrix[i]![j] !== 0 && (j < i - lower || j > i + upper)) {
        return false
      }
    }
  }

  // All elements outside the band are zero
  return true
}
