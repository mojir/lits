export function subtract<T extends number[]>(
  vector1: T,
  vector2: T,
): T {
  return vector1.map((item, index) => item - vector2[index]!) as T
}
