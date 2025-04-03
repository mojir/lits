import type { SequenceDefinition } from '.'

const perfectNumbers = [6, 28, 496, 8128, 33550336, 8589869056, 137438691328]

export const perfectSequence: SequenceDefinition<'perfect'> = {
  'maxLength': perfectNumbers.length,
  'c:perfect-seq': (length) => {
    return perfectNumbers.slice(0, length)
  },
  'c:perfect-nth': n => perfectNumbers[n - 1]!,
  'c:perfect?': n => perfectNumbers.includes(n),
  'c:perfect-take-while': (takeWhile) => {
    const perfect = []
    for (let i = 0; ; i += 1) {
      if (i >= perfectNumbers.length) {
        break
      }
      const value = perfectNumbers[i]!
      if (!takeWhile(value, i)) {
        break
      }
      perfect[i] = value
    }
    return perfect
  },
}
