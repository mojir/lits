import type { SequenceDefinition } from '.'

const pellNumbers = [
  0,
  1,
  2,
  5,
  12,
  29,
  70,
  169,
  408,
  985,
  2378,
  5741,
  13860,
  33461,
  80782,
  195025,
  470832,
  1136689,
  2744210,
  6625109,
  15994428,
  38613965,
  93222358,
  225058681,
  543339720,
  1311738121,
  3166815962,
  7645370045,
  18457556052,
  44560482149,
  107578520350,
  259717522849,
  627013566048,
  1513744654945,
  3654502875938,
  8822750406821,
  21300003689580,
  51422757785981,
  124145519261542,
  299713796309065,
  723573111879672,
  1746860020068409,
  4217293152016490,
]
export const pellSequence: SequenceDefinition<'pell'> = {
  'maxLength': pellNumbers.length,
  'c:pell-seq': (length) => {
    return pellNumbers.slice(0, length)
  },
  'c:pell-nth': n => pellNumbers[n - 1]!,
  'c:pell?': n => pellNumbers.includes(n),
  'c:pell-take-while': (takeWhile) => {
    const pell = []
    for (let i = 0; ; i += 1) {
      if (i >= pellNumbers.length) {
        break
      }
      const value = pellNumbers[i]!
      if (!takeWhile(value, i)) {
        break
      }
      pell[i] = value
    }
    return pell
  },
}
