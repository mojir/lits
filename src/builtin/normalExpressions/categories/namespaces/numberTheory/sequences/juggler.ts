import type { SequenceDefinition } from '.'

export const jugglerSequence: Omit<SequenceDefinition<'juggler'>, 'n:juggler-nth' | 'n:juggler-take-while' | 'n:juggler?'> = {
  'n:juggler-seq': (start) => {
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
