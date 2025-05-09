import type { SequenceDefinition } from '.'

export const thueMorseSequence: SequenceDefinition<'thue-morse'> = {
  'nth:thue-morse-seq': (length) => {
    const thueMorse = []
    for (let i = 0; i < length; i += 1) {
      thueMorse[i] = countSetBits(i) % 2
    }
    return thueMorse
  },
  'nth:thue-morse-take-while': (takeWhile) => {
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
  'nth:thue-morse?': n => n === 1 || n === 0,
}

function countSetBits(num: number): number {
  let count = 0
  while (num) {
    count += num & 1
    num >>= 1
  }
  return count
}
