export function diagonal(matrix: number[][]): number[] {
  const n = matrix.length
  const diag: number[] = []
  for (let i = 0; i < n; i += 1) {
    diag.push(matrix[i]![i]!)
  }
  return diag
}
