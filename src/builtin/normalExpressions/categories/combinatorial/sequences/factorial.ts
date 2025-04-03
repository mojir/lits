import type { SequenceDefinition } from '.'

export const factorialNumbers = [
  1,
  1,
  2,
  6,
  24,
  120,
  720,
  5040,
  40320,
  362880,
  3628800,
  39916800,
  479001600,
  6227020800,
  87178291200,
  1307674368000,
  20922789888000,
  355687428096000,
  6402373705728000,
]
export const factorialSequence: SequenceDefinition<'factorial'> = {
  'maxLength': factorialNumbers.length,
  'c:factorial-seq': (length) => {
    return factorialNumbers.slice(0, length)
  },
  'c:factorial-nth': n => factorialNumbers[n - 1]!,
  'c:factorial?': n => factorialNumbers.includes(n),
  'c:factorial-take-while': (takeWhile) => {
    const factorial = []
    for (let i = 0; ; i += 1) {
      if (i >= factorialNumbers.length) {
        break
      }
      const value = factorialNumbers[i]!
      if (!takeWhile(value, i)) {
        break
      }
      factorial[i] = value
    }
    return factorial
  },
}
