export function subtract(
  vector1: number[],
  vector2: number[],
): number[] {
  return vector1.map((item, index) => item - vector2[index]!)
}
