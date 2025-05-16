import type { NumberTheorySequenceReference } from '.'

export const factorialReference: NumberTheorySequenceReference<'factorial'> = {
  'nth:factorial-seq': {
    title: 'nth:factorial-seq',
    category: 'Number Theory',
    description: 'Generates the factorial sequence up to a specified length.',
    returns: {
      type: 'integer',
      array: true,
    },
    args: {
      length: {
        type: 'integer',
        description: 'The length of the sequence to generate. If not provided, the default is 19 (the maximum length of the pre-calculated factorial numbers).',
      },
    },
    variants: [
      { argumentNames: ['length'] },
      { argumentNames: [] },
    ],
    examples: [
      'nth:factorial-seq(1)',
      'nth:factorial-seq(2)',
      'nth:factorial-seq(3)',
      'nth:factorial-seq(4)',
      'nth:factorial-seq(5)',
      'nth:factorial-seq(10)',
    ],
  },
  'nth:factorial-take-while': {
    title: 'nth:factorial-take-while',
    category: 'Number Theory',
    description: 'Generates the factorial sequence while a condition is met.',
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
      'nth:factorial-take-while(-> $ < 1000)',
    ],
  },
  'nth:factorial-nth': {
    title: 'nth:factorial-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the factorial sequence.',
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
      'nth:factorial-nth(1)',
      'nth:factorial-nth(2)',
      'nth:factorial-nth(3)',
      'nth:factorial-nth(4)',
      'nth:factorial-nth(5)',
      'nth:factorial-nth(10)',
    ],
  },
  'nth:factorial?': {
    title: 'nth:factorial?',
    category: 'Number Theory',
    description: 'Checks if a number is in the factorial sequence.',
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
      'nth:factorial?(1)',
      'nth:factorial?(2)',
      'nth:factorial?(3)',
      'nth:factorial?(4)',
      'nth:factorial?(5)',
      'nth:factorial?(6)',
      'nth:factorial?(7)',
      'nth:factorial?(8)',
      'nth:factorial?(9)',
      'nth:factorial?(3628800)',
    ],
  },
}
