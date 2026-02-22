import type { MaybePromise } from '../../../../utils/maybePromise'
import { chain } from '../../../../utils/maybePromise'
import type { SequenceDefinition } from '.'

function getGolombSeq(n: number): number[] {
  const golomb = [0, 1]
  for (let i = 2; i <= n; i += 1) {
    golomb.push(1 + golomb[i - golomb[golomb[i - 1]!]!]!)
  }
  return golomb.slice(1)
}

function generateGolombSeq(pred: (golombNumber: number, index: number) => MaybePromise<boolean>): MaybePromise<number[]> {
  const golomb = [0, 1]
  return chain(pred(1, 0), (keepFirst) => {
    if (!keepFirst)
      return []
    function loop(i: number): MaybePromise<number[]> {
      const golombNumber = 1 + golomb[i - golomb[golomb[i - 1]!]!]!
      return chain(pred(golombNumber, i - 1), (keep) => {
        if (!keep)
          return golomb.slice(1)
        golomb.push(golombNumber)
        return loop(i + 1)
      })
    }
    return loop(2)
  })
}

export const golombSequence: SequenceDefinition<'golomb'> = {
  'golomb-seq': length => getGolombSeq(length),
  'golomb?': () => true,
  'golomb-take-while': takeWhile => generateGolombSeq(takeWhile),
}
