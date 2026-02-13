import type { NumberTheorySequenceReference } from '.'

export const partitionReference: NumberTheorySequenceReference<'partition'> = {
  'Number-Theory.partition-seq': {
    title: 'Number-Theory.partition-seq',
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
      'let { partition-seq } = import("Number-Theory");\npartition-seq(1)',
      'let { partition-seq } = import("Number-Theory");\npartition-seq(10)',
      'let { partition-seq } = import("Number-Theory");\npartition-seq()',
    ],
  },
  'Number-Theory.partition-take-while': {
    title: 'Number-Theory.partition-take-while',
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
      'let { partition-take-while } = import("Number-Theory");\npartition-take-while(-> $ < 1000)',
    ],
  },
  'Number-Theory.partition-nth': {
    title: 'Number-Theory.partition-nth',
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
      'let { partition-nth } = import("Number-Theory");\npartition-nth(1)',
      'let { partition-nth } = import("Number-Theory");\npartition-nth(5)',
    ],
  },
  'Number-Theory.partition?': {
    title: 'Number-Theory.partition?',
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
      'let { partition? } = import("Number-Theory");\npartition?(0)',
      'let { partition? } = import("Number-Theory");\npartition?(1)',
      'let { partition? } = import("Number-Theory");\npartition?(2)',
      'let { partition? } = import("Number-Theory");\npartition?(3)',
      'let { partition? } = import("Number-Theory");\npartition?(4)',
      'let { partition? } = import("Number-Theory");\npartition?(5)',
    ],
  },
}
