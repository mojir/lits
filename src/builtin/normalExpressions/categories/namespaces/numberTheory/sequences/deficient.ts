import { getProperDivisors } from '../divisors'
import type { SequenceDefinition } from '.'

function isDeficient(num: number): boolean {
  const properDivisors = getProperDivisors(num)
  const sum = properDivisors.reduce((acc, curr) => acc + curr, 0)
  return sum < num
}

export const deficientSequence: SequenceDefinition<'deficient'> = {
  'nth:deficient-seq': (length) => {
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
  'nth:deficient?': n => isDeficient(n),
  'nth:deficient-take-while': (takeWhile) => {
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
