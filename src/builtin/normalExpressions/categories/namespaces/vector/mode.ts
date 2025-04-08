/**
 * Calculates the mode (most frequent value(s)) of a dataset
 * @param values An array of values of any type
 * @returns An array containing the mode(s) of the dataset
 */
export function mode(values: number[]): number[] {
  if (values.length === 0) {
    return []
  }

  // Create a frequency map
  const frequencyMap = new Map<number, number>()

  // Count occurrences of each value
  for (const value of values) {
    frequencyMap.set(value, (frequencyMap.get(value) || 0) + 1)
  }

  // Find the maximum frequency
  let maxFrequency = 0
  for (const frequency of frequencyMap.values()) {
    if (frequency > maxFrequency) {
      maxFrequency = frequency
    }
  }

  // If all values appear only once, there is no mode
  if (maxFrequency === 1) {
    return ([] as number[])
  }

  // Collect all values that appear with the maximum frequency
  const modes: number[] = []
  for (const [value, frequency] of frequencyMap.entries()) {
    if (frequency === maxFrequency) {
      modes.push(value)
    }
  }

  return modes
}
