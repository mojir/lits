export function fromDiagonal(vector: number[]): number[][] {
  const n = vector.length
  const result: number[][] = []
  for (let i = 0; i < n; i += 1) {
    const row: number[] = []
    for (let j = 0; j < n; j += 1) {
      row.push(i === j ? vector[i]! : 0)
    }
    result.push(row)
  }
  return result
}
