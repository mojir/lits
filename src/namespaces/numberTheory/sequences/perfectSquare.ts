import type { SequenceDefinition } from '.'

export const perfectSquareSequence: SequenceDefinition<'perfect-square'> = {
  'perfect-square-seq': (length) => {
    const perfectSquares = []
    for (let i = 1; i <= length; i++) {
      perfectSquares.push(i ** 2)
    }
    return perfectSquares
  },
  'perfect-square?': n => n > 0 && Number.isInteger(Math.sqrt(n)),
  'perfect-square-take-while': (takeWhile) => {
    const perfectSquares = []
    for (let i = 1; ; i++) {
      const value = i ** 2
      if (!takeWhile(value, i)) {
        break
      }
      perfectSquares.push(value)
    }
    return perfectSquares
  },
}
