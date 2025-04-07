import { getProperDivisors } from '../divisors'
import type { SequenceDefinition } from '.'

function isDeficient(num: number): boolean {
  const properDivisors = getProperDivisors(num)
  const sum = properDivisors.reduce((acc, curr) => acc + curr, 0)
  return sum < num
}

export const deficientSequence: SequenceDefinition<'deficient'> = {
  'n:deficient-seq': (length) => {
    const deficients = []
    let num = 1
    while (deficients.length < length) {
      if (isDeficient(num)) {
        deficients.push(num)
      }
      num += 1
    }
    return deficients
  },
  'n:deficient-nth': (n) => {
    let count = 0 // We've already counted 12 and 18
    let candidate = 1 // Start checking from 20
    while (count < n) {
      if (isDeficient(candidate)) {
        count++
        if (count === n) {
          break
        }
      }
      candidate += 1
    }
    return candidate
  },
  'n:deficient?': n => isDeficient(n),
  'n:deficient-take-while': (takeWhile) => {
    const deficients = []
    for (let i = 1; ; i += 1) {
      if (!isDeficient(i)) {
        continue
      }
      if (!takeWhile(i, deficients.length)) {
        break
      }
      deficients.push(i)
    }
    return deficients
  },
}
