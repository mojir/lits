/**
 * Counts occurrences of each integer value in an array of non-negative integers.
 *
 * @param array - Array of non-negative integers to count
 * @param minLength - Minimum length of the output array (default: 0)
 * @param weights - Optional array of weights (same length as input array)
 * @returns An array where index i contains the count of occurrences of i in the input array
 */
export function bincount(
  array: number[],
  minLength: number = 0,
  weights?: number[],
): number[] {
  if (array.length === 0) {
    return Array.from({ length: minLength }, () => 0)
  }

  // Find the maximum value to determine output array size
  const maxValue = Math.max(...array)
  const outputLength = Math.max(maxValue + 1, minLength)
  const counts = Array.from({ length: outputLength }, () => 0)

  // Count occurrences (or sum weights if provided)
  for (let i = 0; i < array.length; i++) {
    const value = Math.floor(array[i]!)

    if (value < outputLength) {
      // If weights provided, add weight; otherwise add 1
      counts[value]! += weights ? weights[i]! : 1
    }
  }

  return counts
}
