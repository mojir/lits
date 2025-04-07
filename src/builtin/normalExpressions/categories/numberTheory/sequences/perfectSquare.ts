import type { SequenceDefinition } from '.'

export const perfectSquareSequence: SequenceDefinition<'perfect-square'> = {
  'n:perfect-square-seq': (length) => {
    const perfectSquares = []
    for (let i = 1; i <= length; i++) {
      perfectSquares.push(i ** 2)
    }
    return perfectSquares
  },
  'n:perfect-square-nth': n => n ** 2,
  'n:perfect-square?': n => n > 0 && Number.isInteger(Math.sqrt(n)),
  'n:perfect-square-take-while': (takeWhile) => {
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
