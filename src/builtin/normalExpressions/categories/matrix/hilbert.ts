/**
 * Creates a Hilbert matrix of size n×n.
 * A Hilbert matrix has elements H[i][j] = 1/(i + j - 1)
 *
 * @param n The size of the square matrix
 * @returns A 2D array representing the Hilbert matrix
 */
export function hilbert(n: number): number[][] {
  // Create an empty n×n matrix
  const matrix: number[][] = []

  // Fill the matrix with Hilbert values
  for (let i = 1; i <= n; i++) {
    const row: number[] = []
    for (let j = 1; j <= n; j++) {
      row.push(1 / (i + j - 1))
    }
    matrix.push(row)
  }

  return matrix
}
