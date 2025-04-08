export function binomialCoefficient(n: number, k: number): number {
  if (k < 0 || k > n)
    return 0

  if (k === 0 || k === n)
    return 1

  let result = 1
  for (let i = 0; i < k; i++)
    result *= (n - i) / (i + 1)

  return result
}
