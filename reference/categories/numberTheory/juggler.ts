import type { NumberTheorySequenceReference } from '.'

export const jugglerReference: Omit<NumberTheorySequenceReference<'juggler'>, 'TEMP-nth.juggler-take-while' | 'TEMP-nth.juggler-nth' | 'TEMP-nth.juggler?'> = {
  'TEMP-nth.juggler-seq': {
    title: 'TEMP-nth.juggler-seq',
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
      'let nt = import("TEMP-nth");\nnt.juggler-seq(3)',
      'let nt = import("TEMP-nth");\nnt.juggler-seq(5)',
    ],
  },
}
