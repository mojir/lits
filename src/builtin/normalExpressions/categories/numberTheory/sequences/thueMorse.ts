import type { SequenceDefinition } from '.'

export const thueMorseSequence: SequenceDefinition<'thue-morse'> = {
  'n:thue-morse-seq': (length) => {
    const thueMorse = []
    for (let i = 0; i < length; i += 1) {
      thueMorse[i] = countSetBits(i) % 2
    }
    return thueMorse
  },
  'n:thue-morse-take-while': (takeWhile) => {
    const thueMorse = []
    for (let i = 0; ; i += 1) {
      const value = countSetBits(i) % 2
      if (!takeWhile(value, i)) {
        break
      }
      thueMorse[i] = value
    }
    return thueMorse
  },
  'n:thue-morse-nth': n => countSetBits(n - 1) % 2,
  'n:thue-morse?': n => n === 1 || n === 0,
}

function countSetBits(num: number): number {
  let count = 0
  while (num) {
    count += num & 1
    num >>= 1
  }
  return count
}
