import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
import type { SequenceDefinition } from '.'

export const perfectCubeSequence: SequenceDefinition<'perfect-cube'> = {
  'perfect-cube-seq': (length) => {
    const perfectcubes = []
    for (let i = 1; i <= length; i++) {
      perfectcubes.push(i ** 3)
    }
    return perfectcubes
  },
  'perfect-cube?': n => n > 0 && Number.isInteger(Math.cbrt(n)),
  'perfect-cube-take-while': (takeWhile) => {
    const perfectcubes: number[] = []
    function loop(i: number): MaybePromise<number[]> {
      const value = i ** 3
      return chain(takeWhile(value, i), (keep) => {
        if (!keep)
          return perfectcubes
        perfectcubes.push(value)
        return loop(i + 1)
      })
    }
    return loop(1)
  },
}
