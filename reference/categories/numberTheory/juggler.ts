import type { NumberTheorySequenceReference } from '.'

export const jugglerReference: Omit<NumberTheorySequenceReference<'juggler'>, 'Number-Theory.juggler-take-while' | 'Number-Theory.juggler-nth' | 'Number-Theory.juggler?'> = {
  'Number-Theory.juggler-seq': {
    title: 'Number-Theory.juggler-seq',
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
      'let { juggler-seq } = import("Number-Theory");\njuggler-seq(3)',
      'let { juggler-seq } = import("Number-Theory");\njuggler-seq(5)',
    ],
  },
}
