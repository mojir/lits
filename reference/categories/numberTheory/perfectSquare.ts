import type { NumberTheorySequenceReference } from '.'

export const perfectSquareReference: NumberTheorySequenceReference<'perfect-square'> = {
  'Number-Theory.perfect-square-seq': {
    title: 'Number-Theory.perfect-square-seq',
    category: 'Number Theory',
    description: 'Generates the perfect square numbers up to a specified length.',
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
      'let { perfect-square-seq } = import("Number-Theory");\nperfect-square-seq(5)',
      'let { perfect-square-seq } = import("Number-Theory");\nperfect-square-seq(20)',
    ],
  },
  'Number-Theory.perfect-square-take-while': {
    title: 'Number-Theory.perfect-square-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect square numbers while a condition is met.',
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
      'let { perfect-square-take-while } = import("Number-Theory");\nperfect-square-take-while(-> $ <= 100)',
    ],
  },
  'Number-Theory.perfect-square-nth': {
    title: 'Number-Theory.perfect-square-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect square numbers.',
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
      'let { perfect-square-nth } = import("Number-Theory");\nperfect-square-nth(1)',
      'let { perfect-square-nth } = import("Number-Theory");\nperfect-square-nth(5)',
    ],
  },
  'Number-Theory.perfect-square?': {
    title: 'Number-Theory.perfect-square?',
    category: 'Number Theory',
    description: 'Checks if a number is a perfect square.',
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
      'let { perfect-square? } = import("Number-Theory");\nperfect-square?(16)',
      'let { perfect-square? } = import("Number-Theory");\nperfect-square?(20)',
    ],
  },
}
