import type { NumberTheorySequenceReference } from '.'

export const collatzReference: Omit<NumberTheorySequenceReference<'collatz'>, 'Number-Theory.collatz-take-while' | 'Number-Theory.collatz-nth' | 'Number-Theory.collatz?'> = {
  'Number-Theory.collatz-seq': {
    title: 'Number-Theory.collatz-seq',
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
      'let { collatz-seq } = import("Number-Theory");\ncollatz-seq(3)',
      'let { collatz-seq } = import("Number-Theory");\ncollatz-seq(11)',
    ],
  },
}
