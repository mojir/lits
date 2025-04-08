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
