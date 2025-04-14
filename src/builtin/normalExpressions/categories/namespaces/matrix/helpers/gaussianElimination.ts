import { approxZero } from '../../../../../../utils'

/**
 * Performs Gaussian elimination on a matrix, transforming it to row echelon form
 *
 * @param matrix - The input matrix
 * @returns A tuple containing the row echelon form matrix and the rank
 */
export function gaussianElismination(matrix: number[][]): [number[][], number] {
  // Create a copy of the matrix to avoid modifying the original
  const m = matrix.map(row => [...row])
  const rows = m.length
  const cols = m[0]!.length

  let rank = 0
  let rowsProcessed = 0

  // Row reduction to echelon form
  for (let col = 0; col < cols; col++) {
    // Find the pivot
    let pivotRow = -1

    for (let row = rowsProcessed; row < rows; row++) {
      if (!approxZero(m[row]![col]!)) {
        pivotRow = row
        break
      }
    }

    if (pivotRow === -1)
      continue // No pivot in this column

    // Increase rank
    rank++

    // Swap rows
    if (pivotRow !== rowsProcessed) {
      [m[pivotRow], m[rowsProcessed]] = [m[rowsProcessed]!, m[pivotRow]!]
    }

    // Normalize pivot row
    const pivotValue = m[rowsProcessed]![col]!

    // only normalize if it's not the last row
    if (rowsProcessed < rows - 1) {
      for (let j = col; j < cols; j++) {
        m[rowsProcessed]![j]! /= pivotValue
      }
    }

    // Eliminate below
    for (let row = rowsProcessed + 1; row < rows; row++) {
      if (!approxZero(m[row]![col]!)) {
        const factor = m[row]![col]!
        for (let j = col; j < cols; j++) {
          m[row]![j]! -= factor * m[rowsProcessed]![j]!
        }
      }
    }

    rowsProcessed++
    if (rowsProcessed === rows)
      break
  }

  return [m, rank]
}
