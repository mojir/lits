import type { NumberTheorySequenceReference } from '.'

export const partitionReference: NumberTheorySequenceReference<'partition'> = {
  'TEMP-nth.partition-seq': {
    title: 'TEMP-nth.partition-seq',
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
      'let { partition-seq } = import("TEMP-nth");\npartition-seq(1)',
      'let { partition-seq } = import("TEMP-nth");\npartition-seq(10)',
      'let { partition-seq } = import("TEMP-nth");\npartition-seq()',
    ],
  },
  'TEMP-nth.partition-take-while': {
    title: 'TEMP-nth.partition-take-while',
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
      'let { partition-take-while } = import("TEMP-nth");\npartition-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.partition-nth': {
    title: 'TEMP-nth.partition-nth',
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
      'let { partition-nth } = import("TEMP-nth");\npartition-nth(1)',
      'let { partition-nth } = import("TEMP-nth");\npartition-nth(5)',
    ],
  },
  'TEMP-nth.partition?': {
    title: 'TEMP-nth.partition?',
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
      'let { partition? } = import("TEMP-nth");\npartition?(0)',
      'let { partition? } = import("TEMP-nth");\npartition?(1)',
      'let { partition? } = import("TEMP-nth");\npartition?(2)',
      'let { partition? } = import("TEMP-nth");\npartition?(3)',
      'let { partition? } = import("TEMP-nth");\npartition?(4)',
      'let { partition? } = import("TEMP-nth");\npartition?(5)',
    ],
  },
}
