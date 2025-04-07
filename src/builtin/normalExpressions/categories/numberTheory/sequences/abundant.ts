import { getProperDivisors } from '../divisors'
import type { SequenceDefinition } from '.'

function isAbundant(num: number): boolean {
  const properDivisors = getProperDivisors(num)
  const sum = properDivisors.reduce((acc, curr) => acc + curr, 0)
  return sum > num
}

export const abundantSequence: SequenceDefinition<'abundant'> = {
  'n:abundant-seq': (length) => {
    const abundants = []
    let num = 2
    while (abundants.length < length) {
      if (isAbundant(num)) {
        abundants.push(num)
      }
      num += 1
    }
    return abundants
  },
  'n:abundant-nth': (n) => {
    if (n === 1)
      return 12 // First abundant number is 12
    if (n === 2)
      return 18 // Second abundant number is 18
    let count = 2 // We've already counted 12 and 18
    let candidate = 20 // Start checking from 20
    while (count < n) {
      if (isAbundant(candidate)) {
        count++
        if (count === n) {
          break
        }
      }
      candidate += 1
    }
    return candidate
  },
  'n:abundant?': n => isAbundant(n),
  'n:abundant-take-while': (takeWhile) => {
    const abundants = []
    for (let i = 2; ; i += 1) {
      if (!isAbundant(i)) {
        continue
      }
      if (!takeWhile(i, abundants.length)) {
        break
      }
      abundants.push(i)
    }
    return abundants
  },
}
