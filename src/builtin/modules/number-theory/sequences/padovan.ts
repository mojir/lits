import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
import type { SequenceDefinition } from '.'

/**
 * Checks if a number is a Padovan number.
 * Padovan numbers follow the recurrence relation: P(n) = P(n-2) + P(n-3) for n >= 3,
 * with initial values P(0) = P(1) = P(2) = 1.
 *
 * The first few Padovan numbers are:
 * 1, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 16, 21, 28, 37, 49, 65, 86, 114, 151, 200, ...
 *
 * @param num - The number to check
 * @returns True if the number is a Padovan number, false otherwise
 */
function isPadovan(num: number): boolean {
  // Padovan numbers are always positive integers
  if (!Number.isInteger(num) || num <= 0) {
    return false
  }

  // Special case: The first three Padovan numbers are all 1
  if (num === 1) {
    return true
  }

  // Pre-calculated Padovan numbers (for efficient lookup, verified for correctness)
  const padovanNumbers = [
    1,
    1,
    1,
    2,
    2,
    3,
    4,
    5,
    7,
    9,
    12,
    16,
    21,
    28,
    37,
    49,
    65,
    86,
    114,
    151,
    200,
    265,
    351,
    465,
    616,
    816,
    1081,
    1432,
    1897,
    2513,
    3329,
    4410,
    5842,
    7739,
    10252,
    13581,
    17991,
    23833,
    31572,
    41824,
    55405,
    73396,
    97229,
  ]

  // Direct lookup for known values
  if (padovanNumbers.includes(num)) {
    return true
  }

  // For numbers larger than our pre-calculated list but within JavaScript's safe range
  if (num > padovanNumbers[padovanNumbers.length - 1]! && num <= Number.MAX_SAFE_INTEGER) {
    // Start with the last three values from our known sequence
    let a = padovanNumbers[padovanNumbers.length - 3]!
    let b = padovanNumbers[padovanNumbers.length - 2]!
    let c = padovanNumbers[padovanNumbers.length - 1]!
    let next

    // Generate Padovan numbers until we either find a match or exceed the input
    while (c < num) {
      next = a + b
      a = b
      b = c
      c = next

      if (c === num) {
        return true
      }

      // Check for numeric overflow/precision issues
      if (!Number.isSafeInteger(c)) {
        return false
      }
    }
  }

  return false
}

export const padovanSequence: SequenceDefinition<'padovan'> = {
  'padovan-seq': (length) => {
    const padovan = [1, 1, 1]
    for (let i = 3; i < length; i += 1) {
      padovan[i] = padovan[i - 2]! + padovan[i - 3]!
    }
    return padovan.slice(0, length)
  },
  'padovan?': n => isPadovan(n),
  'padovan-take-while': (takeWhile) => {
    const padovan: number[] = []
    return chain(takeWhile(1, 0), (keep0) => {
      if (!keep0)
        return padovan
      padovan.push(1)
      return chain(takeWhile(1, 1), (keep1) => {
        if (!keep1)
          return padovan
        padovan.push(1)
        return chain(takeWhile(1, 2), (keep2) => {
          if (!keep2)
            return padovan
          padovan.push(1)
          let a = 1
          let b = 1
          let c = 1
          function loop(i: number): MaybePromise<number[]> {
            const temp = a + b
            a = b
            b = c
            c = temp
            return chain(takeWhile(c, i), (keep) => {
              if (!keep)
                return padovan
              padovan.push(c)
              return loop(i + 1)
            })
          }
          return loop(4)
        })
      })
    })
  },
}
