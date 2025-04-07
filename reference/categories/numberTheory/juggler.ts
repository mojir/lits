import type { NumberTheorySequenceReference } from '.'

export const jugglerReference: Omit<NumberTheorySequenceReference<'juggler'>, 'n:juggler-take-while' | 'n:juggler-nth' | 'n:juggler?'> = {
  'n:juggler-seq': {
    title: 'n:juggler-seq',
    category: 'Number Theory',
    description: 'Generates the Juggler sequence starting from a given integer.',
    linkName: 'c-colon-juggler-seq',
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
      'n:juggler-seq(3)',
      'n:juggler-seq(5)',
    ],
  },
}
