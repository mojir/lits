import type { NumberTheorySequenceReference } from '.'

export const collatzReference: Omit<NumberTheorySequenceReference<'collatz'>, 'n:collatz-take-while' | 'n:collatz-nth' | 'n:collatz?'> = {
  'n:collatz-seq': {
    title: 'n:collatz-seq',
    category: 'Number Theory',
    description: 'Generates the collatz sequence starting from a given integer.',
    linkName: 'c-colon-collatz-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      start: {
        type: 'integer',
        description: 'The starting integer for the collatz sequence.',
      },
    },
    variants: [
      { argumentNames: ['start'] },
    ],
    examples: [
      'n:collatz-seq(3)',
      'n:collatz-seq(11)',
    ],
  },
}
