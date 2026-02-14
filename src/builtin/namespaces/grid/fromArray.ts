/**
 * Creates a grid from a flat array with specified dimensions
 *
 * @param flatArray The flat array of values
 * @param rows Number of rows in the resulting grid
 * @returns A 2D array representing the grid
 */
export function fromArray(flatArray: unknown[], rows: number): unknown[][] {
  // Create the grid
  const grid: unknown[][] = []
  const cols = flatArray.length / rows
  // Reshape the flat array into rows and columns
  for (let i = 0; i < rows; i++) {
    const start = i * cols
    const end = start + cols
    grid.push(flatArray.slice(start, end))
  }

  return grid
}
