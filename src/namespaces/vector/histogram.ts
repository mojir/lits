/**
 * Creates a histogram from an array of numbers, returning bin ranges and counts.
 * Each bin is represented as a tuple of [minValue, maxValue, count].
 *
 * @param vector - Array of numeric values
 * @param bins - Number of bins to divide the data into
 * @returns Array of tuples, each containing [binStart, binEnd, count]
 */
export function calcHistogram(vector: number[], bins: number): [number, number, number][] {
  if (vector.length === 0) {
    // Return empty bins with zero counts if input is empty
    const result: [number, number, number][] = []
    for (let i = 0; i < bins; i++) {
      result.push([0, 0, 0])
    }
    return result
  }

  // Find min and max values
  const min = Math.min(...vector)
  const max = Math.max(...vector)

  // Handle the case where all values are the same
  if (min === max) {
    const result: [number, number, number][] = []
    // Create bins with the same min/max and zero counts
    for (let i = 0; i < bins; i++) {
      result.push([min, min, 0])
    }
    // Put all values in the first bin
    result[0]![2] = vector.length
    return result
  }

  // Calculate bin size
  const binSize = (max - min) / bins

  // Initialize histogram array with bin boundaries and zero counts
  const histogram: [number, number, number][] = []
  for (let i = 0; i < bins; i++) {
    const binStart = min + i * binSize
    const binEnd = i === bins - 1 ? max : min + (i + 1) * binSize
    histogram.push([binStart, binEnd, 0])
  }

  // Count values in each bin
  for (const value of vector) {
    if (value === max) {
      // Place maximum value in the last bin
      histogram[bins - 1]![2] += 1
    }
    else {
      const binIndex = Math.min(
        Math.floor((value - min) / binSize),
        bins - 1,
      )
      histogram[binIndex]![2] += 1
    }
  }

  return histogram
}
