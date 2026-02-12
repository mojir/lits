/**
 * Checks if a vector has no extreme outliers using the IQR method
 * @param vector An array of numbers to check for extreme outliers
 * @returns true if there are no extreme outliers, false otherwise
 */
export function hasOutliers(vector: number[]): boolean {
  return outliers(vector).length > 0
}

export function outliers(vector: number[]): number[] {
  // Handle edge cases
  if (vector.length <= 1)
    return []

  // Sort the vector to calculate quartiles
  const sorted = [...vector].sort((a, b) => a - b)

  // Calculate Q1 (25th percentile)
  const q1Index = Math.floor(sorted.length * 0.25)
  const q1 = sorted.length % 4 === 0
    ? (sorted[q1Index - 1]! + sorted[q1Index]!) / 2
    : sorted[q1Index]!

  // Calculate Q3 (75th percentile)
  const q3Index = Math.floor(sorted.length * 0.75)
  const q3 = sorted.length % 4 === 0
    ? (sorted[q3Index - 1]! + sorted[q3Index]!) / 2
    : sorted[q3Index]!

  // Calculate IQR (Interquartile Range)
  const iqr = q3 - q1

  // Define bounds for outliers (using 1.5*IQR for mild outliers and 3*IQR for extreme outliers)
  const lowerBound = q1 - iqr * 1.5
  const upperBound = q3 + iqr * 1.5

  // Filter the vector to find outliers
  return vector.filter(val => val < lowerBound || val > upperBound)
}
