export function minor(matrix: number[][], row: number, col: number): number[][] {
  const n = matrix.length
  const result: number[][] = []

  for (let i = 0; i < n; i++) {
    if (i !== row) {
      const minorRow: number[] = []
      for (let j = 0; j < n; j++) {
        if (j !== col) {
          minorRow.push(matrix[i]![j]!)
        }
      }
      result.push(minorRow)
    }
  }

  return result
}
