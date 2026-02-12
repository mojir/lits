import { isPrime } from './prime'
import type { SequenceDefinition } from '.'

export function isComposite(num: number): boolean {
  if (num <= 1) {
    return false
  }
  return !isPrime(num)
}

export const compositeSequence: SequenceDefinition<'composite'> = {
  'composite-seq': (length) => {
    const composites = []
    let num = 2
    while (composites.length < length) {
      if (isComposite(num)) {
        composites.push(num)
      }
      num += 1
    }
    return composites
  },
  'composite?': n => isComposite(n),
  'composite-take-while': (takeWhile) => {
    const composites = []
    for (let i = 4; ; i += 1) {
      if (!isComposite(i)) {
        continue
      }
      if (!takeWhile(i, composites.length)) {
        break
      }
      composites.push(i)
    }
    return composites
  },
}
