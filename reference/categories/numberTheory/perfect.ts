import type { NumberTheorySequenceReference } from '.'

export const perfectReference: NumberTheorySequenceReference<'perfect'> = {
  'nth:perfect-seq': {
    title: 'nth:perfect-seq',
    category: 'Number Theory',
    description: 'Generates the perfect numbers up to a specified length.',
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
      'nth:perfect-seq(1)',
      'nth:perfect-seq(5)',
      'nth:perfect-seq()',
    ],
  },
  'nth:perfect-take-while': {
    title: 'nth:perfect-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect numbers while a condition is met.',
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
      'nth:perfect-take-while(-> $ < 1000)',
    ],
  },
  'nth:perfect-nth': {
    title: 'nth:perfect-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect numbers.',
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
      'nth:perfect-nth(1)',
      'nth:perfect-nth(5)',
    ],
  },
  'nth:perfect?': {
    title: 'nth:perfect?',
    category: 'Number Theory',
    description: 'Checks if a number is in the perfect numbers.',
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
      'nth:perfect?(0)',
      'nth:perfect?(1)',
      'nth:perfect?(2)',
      'nth:perfect?(3)',
      'nth:perfect?(4)',
      'nth:perfect?(5)',
      'nth:perfect?(6)',
      'nth:perfect?(7)',
      'nth:perfect?(8)',
      'nth:perfect?(9)',
    ],
  },
}
