import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
import type { SequenceDefinition } from '.'

export const perfectSquareSequence: SequenceDefinition<'perfect-square'> = {
  'perfect-square-seq': (length) => {
    const perfectSquares = []
    for (let i = 1; i <= length; i++) {
      perfectSquares.push(i ** 2)
    }
    return perfectSquares
  },
  'perfect-square?': n => n > 0 && Number.isInteger(Math.sqrt(n)),
  'perfect-square-take-while': (takeWhile) => {
    const perfectSquares: number[] = []
    function loop(i: number): MaybePromise<number[]> {
      const value = i ** 2
      return chain(takeWhile(value, i), (keep) => {
        if (!keep)
          return perfectSquares
        perfectSquares.push(value)
        return loop(i + 1)
      })
    }
    return loop(1)
  },
}
