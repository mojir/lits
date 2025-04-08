import type { NumberTheorySequenceReference } from '.'

export const partitionReference: NumberTheorySequenceReference<'partition'> = {
  'nth:partition-seq': {
    title: 'nth:partition-seq',
    category: 'Number Theory',
    description: 'Generates the partition numbers up to a specified length. If no length is provided, it defaults to 299 (the maximum length of the pre-calculated partition numbers).',
    linkName: 'c-colon-partition-seq',
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
      { argumentNames: [] },
    ],
    examples: [
      'nth:partition-seq(1)',
      'nth:partition-seq(10)',
      'nth:partition-seq()',
    ],
  },
  'nth:partition-take-while': {
    title: 'nth:partition-take-while',
    category: 'Number Theory',
    description: 'Generates the partition numbers while a condition is met.',
    linkName: 'c-colon-partition-take-while',
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
      'nth:partition-take-while(-> $ < 1000)',
    ],
  },
  'nth:partition-nth': {
    title: 'nth:partition-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the partition numbers.',
    linkName: 'c-colon-partition-nth',
    returns: {
      type: 'integer',
    },
    args: {
      n: {
        type: 'integer',
        description: 'The index of the partition number to generate.',
      },
    },
    variants: [
      { argumentNames: ['n'] },
    ],
    examples: [
      'nth:partition-nth(1)',
      'nth:partition-nth(5)',
    ],
  },
  'nth:partition?': {
    title: 'nth:partition?',
    category: 'Number Theory',
    description: 'Checks if a number is in the partition numbers.',
    linkName: 'c-colon-partition?',
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
      'nth:partition?(0)',
      'nth:partition?(1)',
      'nth:partition?(2)',
      'nth:partition?(3)',
      'nth:partition?(4)',
      'nth:partition?(5)',
    ],
  },
}
