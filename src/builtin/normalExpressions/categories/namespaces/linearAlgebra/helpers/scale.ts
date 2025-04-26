export function scale(
  vector: number[],
  scalar: number,
): number[] {
  return vector.map(item => item * scalar)
}
