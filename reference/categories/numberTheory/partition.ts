import type { NumberTheorySequenceReference } from '.'

export const partitionReference: NumberTheorySequenceReference<'partition'> = {
  'n:partition-seq': {
    title: 'n:partition-seq',
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
      'n:partition-seq(1)',
      'n:partition-seq(10)',
      'n:partition-seq()',
    ],
  },
  'n:partition-take-while': {
    title: 'n:partition-take-while',
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
      'n:partition-take-while(-> $ < 1000)',
    ],
  },
  'n:partition-nth': {
    title: 'n:partition-nth',
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
      'n:partition-nth(1)',
      'n:partition-nth(5)',
    ],
  },
  'n:partition?': {
    title: 'n:partition?',
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
      'n:partition?(0)',
      'n:partition?(1)',
      'n:partition?(2)',
      'n:partition?(3)',
      'n:partition?(4)',
      'n:partition?(5)',
    ],
  },
}
