import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
import type { SequenceDefinition } from '.'

export const thueMorseSequence: SequenceDefinition<'thue-morse'> = {
  'thue-morse-seq': (length) => {
    const thueMorse = []
    for (let i = 0; i < length; i += 1) {
      thueMorse[i] = countSetBits(i) % 2
    }
    return thueMorse
  },
  'thue-morse-take-while': (takeWhile) => {
    const thueMorse: number[] = []
    function loop(i: number): MaybePromise<number[]> {
      const value = countSetBits(i) % 2
      return chain(takeWhile(value, i), (keep) => {
        if (!keep)
          return thueMorse
        thueMorse.push(value)
        return loop(i + 1)
      })
    }
    return loop(0)
  },
  'thue-morse?': n => n === 1 || n === 0,
}

function countSetBits(num: number): number {
  let count = 0
  while (num) {
    count += num & 1
    num >>= 1
  }
  return count
}
