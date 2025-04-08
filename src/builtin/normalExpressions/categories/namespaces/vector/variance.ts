export function mean(vector: number[]): number {
  if (vector.length === 0) {
    return 0
  }

  const sum = vector.reduce((acc, val) => acc + val, 0)
  return sum / vector.length
}
