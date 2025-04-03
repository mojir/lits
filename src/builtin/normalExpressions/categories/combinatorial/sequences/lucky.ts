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

function getLuckyNumbers(length: number): number[] {
  const numbers: number[] = []
  for (let i = 1; i <= length * 10; i += 2) { // Generate more than needed
    numbers.push(i)
  }

  // Apply the sieve process
  let idx = 1 // Start from the second element (index 1, which is 3)

  while (idx < numbers.length) {
    const step = numbers[idx]! // Current lucky number

    // Remove every step-th number from the list
    // Count from the beginning each time, and account for changing indices
    let j = 0
    let count = 0
    while (j < numbers.length) {
      count++
      if (count % step === 0) {
        numbers.splice(j, 1)
      }
      else {
        j++ // Only increment if we didn't remove an element
      }
    }

    // Get the new index of the next element (which may have changed)
    idx++
  }

  // Return the first 'length' lucky numbers
  return numbers.slice(0, length)
}

export const luckySequence: SequenceDefinition<'lucky'> = {
  'c:lucky-seq': length => getLuckyNumbers(length),
  'c:lucky-nth': n => getLuckyNumbers(n)[n - 1]!,
  'c:lucky?': n => generateLuckyNumbers(l => l <= n).includes(n),
  'c:lucky-take-while': takeWhile => generateLuckyNumbers(takeWhile),
}
