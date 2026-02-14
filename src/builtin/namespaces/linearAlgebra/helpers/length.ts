export function length(vector: number[]): number {
  return Math.sqrt(vector.reduce((acc, item) => acc + item ** 2, 0))
}
