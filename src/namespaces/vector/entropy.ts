/**
 * Calculates the Shannon entropy of a vector.
 * Entropy measures the amount of uncertainty or randomness in the data.
 *
 * @param vector - An array of values to calculate entropy for
 * @returns The entropy value (in bits) or 0 for empty arrays
 */
export function calculateEntropy<T>(vector: T[]): number {
  // Count occurrences of each value
  const frequencies = new Map<T, number>()
  for (const value of vector) {
    frequencies.set(value, (frequencies.get(value) || 0) + 1)
  }

  // Get the total number of elements
  const total = vector.length

  // Calculate entropy using Shannon's formula
  let entropy = 0
  for (const frequency of frequencies.values()) {
    const probability = frequency / total
    // Skip cases where probability is 0 (log(0) is undefined)
    if (probability > 0) {
      entropy -= probability * Math.log2(probability)
    }
  }

  return entropy
}
