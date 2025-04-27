export function scale<T extends number[]>(
  vector: T,
  scalar: number,
): T {
  return vector.map(item => item * scalar) as T
}
