import type { NumberTheorySequenceReference } from '.'

export const jugglerReference: Omit<NumberTheorySequenceReference<'juggler'>, 'nth:juggler-take-while' | 'nth:juggler-nth' | 'nth:juggler?'> = {
  'nth:juggler-seq': {
    title: 'nth:juggler-seq',
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
      'nth:juggler-seq(3)',
      'nth:juggler-seq(5)',
    ],
  },
}
