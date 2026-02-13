import type { NumberTheorySequenceReference } from '.'

export const collatzReference: Omit<NumberTheorySequenceReference<'collatz'>, 'TEMP-nth.collatz-take-while' | 'TEMP-nth.collatz-nth' | 'TEMP-nth.collatz?'> = {
  'TEMP-nth.collatz-seq': {
    title: 'TEMP-nth.collatz-seq',
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
      'let { collatz-seq } = import("TEMP-nth");\ncollatz-seq(3)',
      'let { collatz-seq } = import("TEMP-nth");\ncollatz-seq(11)',
    ],
  },
}
