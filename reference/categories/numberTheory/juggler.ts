import type { NumberTheorySequenceReference } from '.'

export const jugglerReference: Omit<NumberTheorySequenceReference<'juggler'>, 'nth.juggler-take-while' | 'nth.juggler-nth' | 'nth.juggler?'> = {
  'nth.juggler-seq': {
    title: 'nth.juggler-seq',
    category: 'Number Theory',
    description: 'Generates the Juggler sequence starting from a given integer.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'integer',
        description: 'The starting integer for the Juggler sequence.',
      },
    },
    variants: [
      { argumentNames: ['start'] },
    ],
    examples: [
      'let nt = import("nth");\nnt.juggler-seq(3)',
      'let nt = import("nth");\nnt.juggler-seq(5)',
    ],
  },
}
