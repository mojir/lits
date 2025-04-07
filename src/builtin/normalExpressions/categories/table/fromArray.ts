import type { Any } from '../../../../interface'

/**
 * Creates a matrix from a flat array with specified dimensions
 *
 * @param flatArray The flat array of values
 * @param rows Number of rows in the resulting matrix
 * @param cols Number of columns in the resulting matrix
 * @returns A 2D array representing the matrix
 */
export function fromArray(flatArray: Any[], rows: number, cols: number): Any[][] {
  // Create the matrix
  const table: Any[][] = []

  // Reshape the flat array into rows and columns
  for (let i = 0; i < rows; i++) {
    const start = i * cols
    const end = start + cols
    table.push(flatArray.slice(start, end))
  }

  return table
}
