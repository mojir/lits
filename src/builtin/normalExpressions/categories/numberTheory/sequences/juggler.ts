import type { SequenceDefinition } from '.'

export const jugglerSequence: Omit<SequenceDefinition<'juggler'>, 'n:juggler-nth' | 'n:juggler-take-while' | 'n:juggler?'> = {
  'n:juggler-seq': (start) => {
    let next = start
    const juggler = [next]
    let index = 0
    while (next > 1) {
      next = next % 2 === 0
        ? Math.floor(Math.sqrt(juggler[index]!))
        : Math.floor(juggler[index]! * Math.sqrt(juggler[index]!))
      index += 1
      juggler.push(next)
    }
    return juggler
  },
}
