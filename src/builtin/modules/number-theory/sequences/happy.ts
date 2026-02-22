import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
import type { SequenceDefinition } from '.'

function isHappyNumber(n: number): boolean {
  // A happy number is defined by the following process:
  // 1. Starting with any positive integer, replace the number by the sum of the squares of its digits
  // 2. Repeat until either:
  //    - The number equals 1 (in which case it's a happy number)
  //    - It enters a cycle that doesn't include 1 (in which case it's not a happy number)

  if (n <= 0)
    return false

  // Use a set to detect cycles
  const seen = new Set()

  // Continue until we either reach 1 or detect a cycle
  while (n !== 1 && !seen.has(n)) {
    seen.add(n)
    n = getSumOfSquaredDigits(n)
  }

  // If we reached 1, it's a happy number
  return n === 1
}

function getSumOfSquaredDigits(n: number): number {
  let sum = 0

  while (n > 0) {
    const digit = n % 10
    sum += digit * digit
    n = Math.floor(n / 10)
  }

  return sum
}

export const happySequence: SequenceDefinition<'happy'> = {
  'happy-seq': (length) => {
    const happyNumbers: number[] = []
    for (let i = 1; happyNumbers.length < length; i++) {
      let n = i
      const seen = new Set<number>()
      while (n !== 1 && !seen.has(n)) {
        seen.add(n)
        n = String(n)
          .split('')
          .reduce((sum, digit) => sum + Number(digit) ** 2, 0)
      }
      if (n === 1)
        happyNumbers.push(i)
    }
    return happyNumbers
  },
  'happy?': n => isHappyNumber(n),
  'happy-take-while': (takeWhile) => {
    const happyNumbers: number[] = []
    function loop(i: number): MaybePromise<number[]> {
      if (!isHappyNumber(i))
        return loop(i + 1)
      return chain(takeWhile(i, happyNumbers.length), (keep) => {
        if (!keep)
          return happyNumbers
        happyNumbers.push(i)
        return loop(i + 1)
      })
    }
    return loop(1)
  },
}
