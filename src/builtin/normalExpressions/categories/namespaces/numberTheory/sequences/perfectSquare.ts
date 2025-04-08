import type { SequenceDefinition } from '.'

export const perfectSquareSequence: SequenceDefinition<'perfect-square'> = {
  'nth:perfect-square-seq': (length) => {
    const perfectSquares = []
    for (let i = 1; i <= length; i++) {
      perfectSquares.push(i ** 2)
    }
    return perfectSquares
  },
  'nth:perfect-square?': n => n > 0 && Number.isInteger(Math.sqrt(n)),
  'nth:perfect-square-take-while': (takeWhile) => {
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
