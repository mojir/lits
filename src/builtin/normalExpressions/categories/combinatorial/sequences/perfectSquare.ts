import type { SequenceDefinition } from '.'

export const perfectSquareSequence: SequenceDefinition<'perfect-square'> = {
  'c:perfect-square-seq': (length) => {
    const perfectSquares = []
    for (let i = 1; i <= length; i++) {
      perfectSquares.push(i ** 2)
    }
    return perfectSquares
  },
  'c:perfect-square-nth': n => n ** 2,
  'c:perfect-square?': n => n > 0 && Number.isInteger(Math.sqrt(n)),
  'c:perfect-square-take-while': (takeWhile) => {
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
