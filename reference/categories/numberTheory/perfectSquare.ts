import type { NumberTheorySequenceReference } from '.'

export const perfectSquareReference: NumberTheorySequenceReference<'perfect-square'> = {
  'n:perfect-square-seq': {
    title: 'n:perfect-square-seq',
    category: 'Number Theory',
    description: 'Generates the perfect square numbers up to a specified length.',
    linkName: 'c-colon-perfect-square-seq',
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
      'n:perfect-square-seq(5)',
      'n:perfect-square-seq(20)',
    ],
  },
  'n:perfect-square-take-while': {
    title: 'n:perfect-square-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect square numbers while a condition is met.',
    linkName: 'c-colon-perfect-square-take-while',
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
      'n:perfect-square-take-while(-> $ <= 100)',
    ],
  },
  'n:perfect-square-nth': {
    title: 'n:perfect-square-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect square numbers.',
    linkName: 'c-colon-perfect-square-nth',
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
      'n:perfect-square-nth(1)',
      'n:perfect-square-nth(5)',
    ],
  },
  'n:perfect-square?': {
    title: 'n:perfect-square?',
    category: 'Number Theory',
    description: 'Checks if a number is a perfect square.',
    linkName: 'c-colon-perfect-square',
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
      'n:perfect-square?(16)',
      'n:perfect-square?(20)',
    ],
  },
}
