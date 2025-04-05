import type { SequenceDefinition } from '.'

export const collatzSequence: Omit<SequenceDefinition<'collatz'>, 'c:collatz-nth' | 'c:collatz-take-while' | 'c:collatz?'> = {
  'c:collatz-seq': (start) => {
    let x = start
    const collatz = [x]
    while (x !== 1) {
      if (x % 2 === 0) {
        x /= 2
      }
      else {
        x = 3 * x + 1
      }
      collatz.push(x)
    }
    return collatz
  },
}
