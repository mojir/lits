/**
 * Calculate the percentile value from an array of numbers
 * @param data Array of numbers to calculate percentile from
 * @param percentile The percentile to calculate (0-100)
 * @returns The value at the specified percentile
 */
export function calcPercentile(data: number[], percentile: number): number {
  if (percentile < 0 || percentile > 100) {
    throw new Error('Percentile must be between 0 and 100')
  }

  if (data.length === 0) {
    throw new Error('Data array cannot be empty')
  }

  // Sort the data in ascending order
  const sortedData = [...data].sort((a, b) => a - b)

  // If percentile is 0, return the minimum value
  if (percentile === 0) {
    return sortedData[0]!
  }

  // If percentile is 100, return the maximum value
  if (percentile === 100) {
    return sortedData[sortedData.length - 1]!
  }

  // Calculate the index
  const index = (percentile / 100) * (sortedData.length - 1)

  // If index is an integer, return the value at that index
  if (Number.isInteger(index)) {
    return sortedData[index]!
  }

  // Otherwise, interpolate between the two adjacent values
  const lowerIndex = Math.floor(index)
  const upperIndex = Math.ceil(index)
  const weight = index - lowerIndex

  return sortedData[lowerIndex]! * (1 - weight) + sortedData[upperIndex]! * weight
}
