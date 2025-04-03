import type { SequenceDefinition } from '.'

const catalanNumbers = [
  1,
  2,
  5,
  14,
  42,
  132,
  429,
  1430,
  4862,
  16796,
  58786,
  208012,
  742900,
  2674440,
  9694845,
  35357670,
  129644790,
  477638700,
  1767263190,
  6564120420,
  24466267020,
  91482563640,
  343059613650,
  1289904147324,
  4861946401452,
  18367353072152,
  69533550916004,
  263747951750360,
  1002242216651368,
  3814986502092304,
]
export const catalanSequence: SequenceDefinition<'catalan'> = {
  'maxLength': catalanNumbers.length,
  'c:catalan-seq': (length) => {
    return catalanNumbers.slice(0, length)
  },
  'c:catalan-nth': n => catalanNumbers[n - 1]!,
  'c:catalan?': n => catalanNumbers.includes(n),
  'c:catalan-take-while': (takeWhile) => {
    const catalan = []
    for (let i = 0; ; i += 1) {
      if (i >= catalanNumbers.length) {
        break
      }
      const value = catalanNumbers[i]!
      if (!takeWhile(value, i)) {
        break
      }
      catalan.push(value)
    }
    return catalan
  },
}
