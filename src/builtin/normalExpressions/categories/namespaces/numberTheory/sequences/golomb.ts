import type { SequenceDefinition } from '.'

function getGolombSeq(n: number): number[] {
  const golomb = [0, 1]
  for (let i = 2; i <= n; i += 1) {
    golomb.push(1 + golomb[i - golomb[golomb[i - 1]!]!]!)
  }
  return golomb.slice(1)
}

function generateGolombSeq(pred: (golombNumber: number, index: number) => boolean): number[] {
  if (!pred(1, 0)) {
    return []
  }
  const golomb = [0, 1]
  for (let i = 2; ; i++) {
    const golombNumber = 1 + golomb[i - golomb[golomb[i - 1]!]!]!
    if (!pred(golombNumber, i - 1)) {
      break
    }
    golomb.push(golombNumber)
  }
  return golomb.slice(1)
}

export const golombSequence: SequenceDefinition<'golomb'> = {
  'nth:golomb-seq': length => getGolombSeq(length),
  'nth:golomb?': () => true,
  'nth:golomb-take-while': takeWhile => generateGolombSeq(takeWhile),
}
