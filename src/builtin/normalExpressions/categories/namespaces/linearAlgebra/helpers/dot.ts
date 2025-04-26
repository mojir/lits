export function dot(
  vector1: number[],
  vector2: number[],
): number {
  return vector1.reduce((acc, item, index) => acc + item * vector2[index]!, 0)
}
