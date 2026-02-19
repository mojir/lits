/**
 * Creates a band matrix with specified lower and upper bandwidths
 *
 * @param n Size of the square matrix
 * @param lband Lower bandwidth (number of non-zero diagonals below main diagonal)
 * @param uband Upper bandwidth (number of non-zero diagonals above main diagonal)
 * @returns A 2D array representing the band matrix with 1s in the band and 0s elsewhere
 */
export function band(n: number, lband: number, uband: number): number[][] {
  // Create an n×n matrix filled with zeros
  const matrix: number[][] = Array.from({ length: n }, () => Array.from({ length: n }, () => 0))

  // Fill the band with 1s
  for (let i = 0; i < n; i++) {
    for (let j = Math.max(0, i - lband); j <= Math.min(n - 1, i + uband); j++) {
      matrix[i]![j] = 1
    }
  }

  return matrix
}
