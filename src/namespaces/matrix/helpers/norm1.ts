// Assuming a matrix is represented as a 2D array
export function norm1(matrix: number[][]): number {
  const numRows = matrix.length
  const numCols = matrix[0]!.length

  let maxColSum = 0

  // Iterate through each column
  for (let j = 0; j < numCols; j += 1) {
    let colSum = 0

    // Sum the absolute values of all elements in this column
    for (let i = 0; i < numRows; i += 1) {
      colSum += Math.abs(matrix[i]![j]!)
    }

    // Update the maximum column sum if necessary
    maxColSum = Math.max(maxColSum, colSum)
  }

  return maxColSum
}
