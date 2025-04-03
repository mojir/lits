import type { SequenceDefinition } from '.'

const mersenneNumbers = [3, 7, 31, 127, 2047, 8191, 131071, 524287, 2147483647]

export const mersenneSequence: SequenceDefinition<'mersenne'> = {
  'maxLength': mersenneNumbers.length,
  'c:mersenne-seq': (length) => {
    return mersenneNumbers.slice(0, length)
  },
  'c:mersenne-nth': n => mersenneNumbers[n - 1]!,
  'c:mersenne?': n => mersenneNumbers.includes(n),
  'c:mersenne-take-while': (takeWhile) => {
    const mersenne = []
    for (let i = 0; ; i += 1) {
      if (i >= mersenneNumbers.length) {
        break
      }
      const value = mersenneNumbers[i]!
      if (!takeWhile(value, i)) {
        break
      }
      mersenne[i] = value
    }
    return mersenne
  },
}
