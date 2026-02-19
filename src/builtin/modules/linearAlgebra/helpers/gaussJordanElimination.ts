import { approxZero } from '../../../../utils'

/**
 * Performs Gauss-Jordan elimination on a matrix, transforming it to reduced row echelon form
 *
 * @param matrix - The input matrix
 * @returns A tuple containing the reduced row echelon form matrix and the rank
 */
export function gaussJordanElimination(matrix: number[][]): [number[][], number] {
  // Create a copy of the matrix to avoid modifying the original
  const m = matrix.map(row => [...row])
  const rows = m.length
  const cols = m[0]!.length

  let rank = 0
  let rowsProcessed = 0

  // Row reduction to reduced row echelon form
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
    rank += 1

    // Swap rows
    if (pivotRow !== rowsProcessed) {
      [m[pivotRow], m[rowsProcessed]] = [m[rowsProcessed]!, m[pivotRow]!]
    }

    // Get the pivot value
    const pivotValue = m[rowsProcessed]![col]!

    // Normalize the pivot row (always, for RREF)
    for (let j = col; j < cols; j++) {
      m[rowsProcessed]![j]! /= pivotValue
    }

    // Eliminate above and below (full Gauss-Jordan)
    for (let row = 0; row < rows; row++) {
      if (row !== rowsProcessed && !approxZero(m[row]![col]!)) {
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
