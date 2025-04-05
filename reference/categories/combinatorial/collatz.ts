import type { CombinatorialSequenceReference } from '.'

export const collatzReference: Omit<CombinatorialSequenceReference<'collatz'>, 'c:collatz-take-while' | 'c:collatz-nth' | 'c:collatz?'> = {
  'c:collatz-seq': {
    title: 'c:collatz-seq',
    category: 'Combinatorial',
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
      'c:collatz-seq(3)',
      'c:collatz-seq(11)',
    ],
  },
}
