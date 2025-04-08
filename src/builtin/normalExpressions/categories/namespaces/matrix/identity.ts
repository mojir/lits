export function identity(number: number): number[][] {
  const result: number[][] = []
  for (let i = 0; i < number; i += 1) {
    const row: number[] = []
    for (let j = 0; j < number; j += 1) {
      row.push(i === j ? 1 : 0)
    }
    result.push(row)
  }
  return result
}
