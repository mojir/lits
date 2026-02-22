import { getProperDivisors } from '../divisors'
import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
import type { SequenceDefinition } from '.'

function isAbundant(num: number): boolean {
  const properDivisors = getProperDivisors(num)
  const sum = properDivisors.reduce((acc, curr) => acc + curr, 0)
  return sum > num
}

export const abundantSequence: SequenceDefinition<'abundant'> = {
  'abundant-seq': (length) => {
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
  'abundant?': n => isAbundant(n),
  'abundant-take-while': (takeWhile) => {
    const abundants: number[] = []
    function loop(i: number): MaybePromise<number[]> {
      if (!isAbundant(i))
        return loop(i + 1)
      return chain(takeWhile(i, abundants.length), (keep) => {
        if (!keep)
          return abundants
        abundants.push(i)
        return loop(i + 1)
      })
    }
    return loop(2)
  },
}
