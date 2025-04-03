/**
 * Checks if a number is a perfect square.
 *
 * @param {number} n - The number to check
 * @return {boolean} - True if n is a perfect square, false otherwise
 */
export function isPerfectSquare(n: number): boolean {
  const sqrt = Math.sqrt(n)
  return Math.floor(sqrt) === sqrt
}
