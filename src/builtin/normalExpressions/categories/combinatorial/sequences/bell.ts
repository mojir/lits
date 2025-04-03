import type { SequenceDefinition } from '.'

// Pre-calculated Bell numbers (for efficient lookup)
// Only including values up to the safe integer limit in JavaScript
const bellNumbers = [
  1,
  2,
  5,
  15,
  52,
  203,
  877,
  4140,
  21147,
  115975,
  678570,
  4213597,
  27644437,
  190899322,
  1382958545,
  10480142147,
  82864869804,
  682076806159,
  5832742205057,
  51724158235372,
  474869816156751,
  4506715738447323,
]

export const bellSequence: SequenceDefinition<'bell'> = {
  'maxLength': bellNumbers.length,
  'c:bell-seq': (length) => {
    return bellNumbers.slice(0, length)
  },
  'c:bell-nth': n => bellNumbers[n - 1]!,
  'c:bell?': n => bellNumbers.includes(n),
  'c:bell-take-while': (takeWhile) => {
    const bell = []
    for (let i = 0; ; i += 1) {
      if (i >= bellNumbers.length) {
        break
      }
      const value = bellNumbers[i]!
      if (!takeWhile(value, i)) {
        break
      }
      bell.push(value)
    }
    return bell
  },
}
