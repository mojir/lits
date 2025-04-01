/**
 * Calculates the trace of a square matrix.
 * The trace is defined as the sum of the elements on the main diagonal.
 *
 * @param matrix - A 2D array representing a square matrix.
 * @returns The trace of the matrix.
 */
export function trace(matrix: number[][]): number {
  return matrix.reduce((sum, row, i) => sum + row[i]!, 0)
}
