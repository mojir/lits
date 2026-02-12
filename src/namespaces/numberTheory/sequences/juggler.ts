import type { SequenceDefinition } from '.'

export const jugglerSequence: Omit<SequenceDefinition<'juggler'>, 'juggler-nth' | 'juggler-take-while' | 'juggler?'> = {
  'juggler-seq': (start) => {
    let next = start
    const juggler = [next]

    while (next > 1) {
      next = next % 2 === 0
        ? Math.floor(Math.sqrt(next))
        : Math.floor(next ** (3 / 2))
      juggler.push(next)
    }
    return juggler
  },
  'noNth': true,
}
