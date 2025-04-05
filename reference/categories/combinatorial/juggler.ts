import type { CombinatorialSequenceReference } from '.'

export const jugglerReference: Omit<CombinatorialSequenceReference<'juggler'>, 'c:juggler-take-while' | 'c:juggler-nth' | 'c:juggler?'> = {
  'c:juggler-seq': {
    title: 'c:juggler-seq',
    category: 'Combinatorial',
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
      'c:juggler-seq(3)',
      'c:juggler-seq(5)',
    ],
  },
}
