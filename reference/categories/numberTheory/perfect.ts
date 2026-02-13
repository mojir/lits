import type { NumberTheorySequenceReference } from '.'

export const perfectReference: NumberTheorySequenceReference<'perfect'> = {
  'Number-Theory.perfect-seq': {
    title: 'Number-Theory.perfect-seq',
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
      'let { perfect-seq } = import("Number-Theory");\nperfect-seq(1)',
      'let { perfect-seq } = import("Number-Theory");\nperfect-seq(5)',
      'let { perfect-seq } = import("Number-Theory");\nperfect-seq()',
    ],
  },
  'Number-Theory.perfect-take-while': {
    title: 'Number-Theory.perfect-take-while',
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
      'let { perfect-take-while } = import("Number-Theory");\nperfect-take-while(-> $ < 1000)',
    ],
  },
  'Number-Theory.perfect-nth': {
    title: 'Number-Theory.perfect-nth',
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
      'let { perfect-nth } = import("Number-Theory");\nperfect-nth(1)',
      'let { perfect-nth } = import("Number-Theory");\nperfect-nth(5)',
    ],
  },
  'Number-Theory.perfect?': {
    title: 'Number-Theory.perfect?',
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
      'let { perfect? } = import("Number-Theory");\nperfect?(0)',
      'let { perfect? } = import("Number-Theory");\nperfect?(1)',
      'let { perfect? } = import("Number-Theory");\nperfect?(2)',
      'let { perfect? } = import("Number-Theory");\nperfect?(3)',
      'let { perfect? } = import("Number-Theory");\nperfect?(4)',
      'let { perfect? } = import("Number-Theory");\nperfect?(5)',
      'let { perfect? } = import("Number-Theory");\nperfect?(6)',
      'let { perfect? } = import("Number-Theory");\nperfect?(7)',
      'let { perfect? } = import("Number-Theory");\nperfect?(8)',
      'let { perfect? } = import("Number-Theory");\nperfect?(9)',
    ],
  },
}
