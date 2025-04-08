import type { SequenceDefinition } from '.'

/**
 * Generates lucky numbers while the predicate function returns true.
 *
 * @param predicate - Function that tests if we should continue generating numbers.
 *                    Takes the current lucky number and index as parameters.
 * @returns An array of lucky numbers
 */
function generateLuckyNumbers(
  predicate: (luckyNumber: number, index: number) => boolean,
): number[] {
  // Start with counting from 1
  const numbers: number[] = []
  for (let i = 1; i <= 2000; i++) {
    numbers.push(i)
  }

  // First step: remove all even numbers (keep 1)
  let filteredNumbers: number[] = [1]
  for (let i = 1; i < numbers.length; i++) {
    if (numbers[i]! % 2 !== 0) {
      filteredNumbers.push(numbers[i]!)
    }
  }

  const luckyNumbers: number[] = [1] // 1 is always the first lucky number
  let count = 1

  // Check if we should continue after the first number
  if (!predicate(1, 0)) {
    return []
  }

  // Continue the sieve process
  let index = 1 // Start with the second element (index 1, which is 3)

  while (index < filteredNumbers.length) {
    // Get the current lucky number
    const luckyNumber = filteredNumbers[index]!

    // Check if we should continue
    if (!predicate(luckyNumber, count)) {
      break
    }

    // Add to result
    luckyNumbers.push(luckyNumber)
    count++

    // Apply the sieve
    const step = luckyNumber
    const newFiltered: number[] = []

    for (let i = 0; i < filteredNumbers.length; i++) {
      if ((i + 1) % step !== 0) { // Keep numbers not at positions divisible by step
        newFiltered.push(filteredNumbers[i]!)
      }
    }

    filteredNumbers = newFiltered
    index++

    // If we're running low on numbers, extend the sequence
    if (index >= filteredNumbers.length - 5) {
      const lastNum = filteredNumbers[filteredNumbers.length - 1]!
      let next = lastNum + 2

      while (filteredNumbers.length < index + 1000) {
        filteredNumbers.push(next)
        next += 2
      }
    }
  }

  return luckyNumbers
}

/**
 * Generates lucky numbers up to a specified length or count
 *
 * Lucky numbers are a subset of integers defined by a specific sieving process:
 * 1. Start with all positive integers: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...
 * 2. Keep 1, delete every 2nd number: 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, ...
 * 3. The second remaining number is 3, so keep it and delete every 3rd number: 1, 3, 7, 9, 13, 15, 19, ...
 * 4. The third remaining number is 7, so keep it and delete every 7th number
 * 5. Continue this process to get all lucky numbers
 *
 * @param count - The number of lucky numbers to generate
 * @returns An array containing the first 'count' lucky numbers
 */
function getLuckyNumbers(count: number): number[] {
  // Step 1: Start with all odd numbers (we skip the first elimination step since we know
  // the first sieve removes all even numbers)
  const numbers: number[] = []
  let n = 1
  // Generate enough odd numbers to ensure we'll have 'count' lucky numbers after sieving
  // The factor depends on how many numbers we expect to be eliminated
  // For larger counts, we need a higher factor to ensure we have enough numbers
  const factor = count < 100 ? 20 : 30
  const initialSize = count * factor

  while (numbers.length < initialSize) {
    numbers.push(n)
    n += 2
  }

  // Step 2 and beyond: Apply the lucky number sieve
  let sieveIndex = 1 // Start at index 1 (the second element which is 3)

  while (sieveIndex < numbers.length && sieveIndex < count) {
    const sieveValue = numbers[sieveIndex]!

    // Remove every sieveValue-th number
    // This is an optimization over creating a new array each time
    let j = 0
    for (let i = 0; i < numbers.length; i++) {
      if ((i + 1) % sieveValue !== 0) {
        numbers[j++] = numbers[i]!
      }
    }
    numbers.length = j // Truncate the array

    // Only increment sieveIndex if it's still within the new array bounds
    if (sieveIndex < numbers.length) {
      sieveIndex++
    }
  }

  // Return the requested number of lucky numbers
  return numbers.slice(0, count)
}

export const luckySequence: SequenceDefinition<'lucky'> = {
  'nth:lucky-seq': length => getLuckyNumbers(length),
  'nth:lucky?': n => generateLuckyNumbers(l => l <= n).includes(n),
  'nth:lucky-take-while': takeWhile => generateLuckyNumbers(takeWhile),
}
