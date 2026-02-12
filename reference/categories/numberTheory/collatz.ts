import type { NumberTheorySequenceReference } from '.'

export const collatzReference: Omit<NumberTheorySequenceReference<'collatz'>, 'nth.collatz-take-while' | 'nth.collatz-nth' | 'nth.collatz?'> = {
  'nth.collatz-seq': {
    title: 'nth.collatz-seq',
    category: 'Number Theory',
    description: 'Generates the collatz sequence starting from a given integer.',
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
      'let nt = import("nth");\nnt.collatz-seq(3)',
      'let nt = import("nth");\nnt.collatz-seq(11)',
    ],
  },
}
