import type { CombinatorialSequenceReference } from '.'

export const partitionReference: CombinatorialSequenceReference<'partition'> = {
  'c:partition-seq': {
    title: 'c:partition-seq',
    category: 'Combinatorial',
    description: 'Generates the partition numbers up to a specified length.',
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
    ],
    examples: [
      'c:partition-seq(1)',
      'c:partition-seq(10)',
      'c:partition-seq()',
    ],
  },
  'c:partition-take-while': {
    title: 'c:partition-take-while',
    category: 'Combinatorial',
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
      'c:partition-take-while(-> $ < 1000)',
    ],
  },
  'c:partition-nth': {
    title: 'c:partition-nth',
    category: 'Combinatorial',
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
      'c:partition-nth(1)',
      'c:partition-nth(5)',
    ],
  },
  'c:partition?': {
    title: 'c:partition?',
    category: 'Combinatorial',
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
      'c:partition?(0)',
      'c:partition?(1)',
      'c:partition?(2)',
      'c:partition?(3)',
      'c:partition?(4)',
      'c:partition?(5)',
    ],
  },
}
