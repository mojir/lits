import type { NumberTheorySequenceReference } from '.'

export const perfectReference: NumberTheorySequenceReference<'perfect'> = {
  'TEMP-nth.perfect-seq': {
    title: 'TEMP-nth.perfect-seq',
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
      'let { perfect-seq } = import("TEMP-nth");\nperfect-seq(1)',
      'let { perfect-seq } = import("TEMP-nth");\nperfect-seq(5)',
      'let { perfect-seq } = import("TEMP-nth");\nperfect-seq()',
    ],
  },
  'TEMP-nth.perfect-take-while': {
    title: 'TEMP-nth.perfect-take-while',
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
      'let { perfect-take-while } = import("TEMP-nth");\nperfect-take-while(-> $ < 1000)',
    ],
  },
  'TEMP-nth.perfect-nth': {
    title: 'TEMP-nth.perfect-nth',
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
      'let { perfect-nth } = import("TEMP-nth");\nperfect-nth(1)',
      'let { perfect-nth } = import("TEMP-nth");\nperfect-nth(5)',
    ],
  },
  'TEMP-nth.perfect?': {
    title: 'TEMP-nth.perfect?',
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
      'let { perfect? } = import("TEMP-nth");\nperfect?(0)',
      'let { perfect? } = import("TEMP-nth");\nperfect?(1)',
      'let { perfect? } = import("TEMP-nth");\nperfect?(2)',
      'let { perfect? } = import("TEMP-nth");\nperfect?(3)',
      'let { perfect? } = import("TEMP-nth");\nperfect?(4)',
      'let { perfect? } = import("TEMP-nth");\nperfect?(5)',
      'let { perfect? } = import("TEMP-nth");\nperfect?(6)',
      'let { perfect? } = import("TEMP-nth");\nperfect?(7)',
      'let { perfect? } = import("TEMP-nth");\nperfect?(8)',
      'let { perfect? } = import("TEMP-nth");\nperfect?(9)',
    ],
  },
}
