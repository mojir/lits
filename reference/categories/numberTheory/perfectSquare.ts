import type { NumberTheorySequenceReference } from '.'

export const perfectSquareReference: NumberTheorySequenceReference<'perfect-square'> = {
  'nth.perfect-square-seq': {
    title: 'nth.perfect-square-seq',
    category: 'Number Theory',
    description: 'Generates the perfect square numbers up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate.',
      },
    },
    variants: [
      { argumentNames: ['length'] },
    ],
    examples: [
      'nth.perfect-square-seq(5)',
      'nth.perfect-square-seq(20)',
    ],
  },
  'nth.perfect-square-take-while': {
    title: 'nth.perfect-square-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect square numbers while a condition is met.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      takeWhile: {
        type: 'function',
        description: 'A function that takes an integer and an index and returns a boolean.',
      },
    },
    variants: [
      { argumentNames: ['takeWhile'] },
    ],
    examples: [
      'nth.perfect-square-take-while(-> $ <= 100)',
    ],
  },
  'nth.perfect-square-nth': {
    title: 'nth.perfect-square-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect square numbers.',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the term to generate.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'nth.perfect-square-nth(1)',
      'nth.perfect-square-nth(5)',
    ],
  },
  'nth.perfect-square?': {
    title: 'nth.perfect-square?',
    category: 'Number Theory',
    description: 'Checks if a number is a perfect square.',
    returns: {
      type: 'boolean',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The number to check.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'nth.perfect-square?(16)',
      'nth.perfect-square?(20)',
    ],
  },
}
