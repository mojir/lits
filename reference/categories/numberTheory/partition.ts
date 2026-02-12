import type { NumberTheorySequenceReference } from '.'

export const partitionReference: NumberTheorySequenceReference<'partition'> = {
  'nth.partition-seq': {
    title: 'nth.partition-seq',
    category: 'Number Theory',
    description: 'Generates the partition numbers up to a specified length. If no length is provided, it defaults to 299 (the maximum length of the pre-calculated partition numbers).',
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
      'let nt = import("nth");\nnt.partition-seq(1)',
      'let nt = import("nth");\nnt.partition-seq(10)',
      'let nt = import("nth");\nnt.partition-seq()',
    ],
  },
  'nth.partition-take-while': {
    title: 'nth.partition-take-while',
    category: 'Number Theory',
    description: 'Generates the partition numbers while a condition is met.',
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
      'let nt = import("nth");\nnt.partition-take-while(-> $ < 1000)',
    ],
  },
  'nth.partition-nth': {
    title: 'nth.partition-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the partition numbers.',
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
      'let nt = import("nth");\nnt.partition-nth(1)',
      'let nt = import("nth");\nnt.partition-nth(5)',
    ],
  },
  'nth.partition?': {
    title: 'nth.partition?',
    category: 'Number Theory',
    description: 'Checks if a number is in the partition numbers.',
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
      'let nt = import("nth");\nnt.partition?(0)',
      'let nt = import("nth");\nnt.partition?(1)',
      'let nt = import("nth");\nnt.partition?(2)',
      'let nt = import("nth");\nnt.partition?(3)',
      'let nt = import("nth");\nnt.partition?(4)',
      'let nt = import("nth");\nnt.partition?(5)',
    ],
  },
}
