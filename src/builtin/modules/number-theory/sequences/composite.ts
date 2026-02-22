import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
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
    const composites: number[] = []
    function loop(i: number): MaybePromise<number[]> {
      if (!isComposite(i))
        return loop(i + 1)
      return chain(takeWhile(i, composites.length), (keep) => {
        if (!keep)
          return composites
        composites.push(i)
        return loop(i + 1)
      })
    }
    return loop(4)
  },
}
