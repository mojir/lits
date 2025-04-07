import type { NumberTheorySequenceReference } from '.'

export const perfectReference: NumberTheorySequenceReference<'perfect'> = {
  'n:perfect-seq': {
    title: 'n:perfect-seq',
    category: 'Number Theory',
    description: 'Generates the perfect numbers up to a specified length.',
    linkName: 'c-colon-perfect-seq',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If no length is provided, it defaults to 7 (the maximum length of the pre-calculated perfect numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'n:perfect-seq(1)',
      'n:perfect-seq(5)',
      'n:perfect-seq()',
    ],
  },
  'n:perfect-take-while': {
    title: 'n:perfect-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect numbers while a condition is met.',
    linkName: 'c-colon-perfect-take-while',
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
      'n:perfect-take-while(-> $ < 1000)',
    ],
  },
  'n:perfect-nth': {
    title: 'n:perfect-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect numbers.',
    linkName: 'c-colon-perfect-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the perfect number to generate.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'n:perfect-nth(1)',
      'n:perfect-nth(5)',
    ],
  },
  'n:perfect?': {
    title: 'n:perfect?',
    category: 'Number Theory',
    description: 'Checks if a number is in the perfect numbers.',
    linkName: 'c-colon-perfect-question-mark',
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
      'n:perfect?(0)',
      'n:perfect?(1)',
      'n:perfect?(2)',
      'n:perfect?(3)',
      'n:perfect?(4)',
      'n:perfect?(5)',
      'n:perfect?(6)',
      'n:perfect?(7)',
      'n:perfect?(8)',
      'n:perfect?(9)',
    ],
  },
}
