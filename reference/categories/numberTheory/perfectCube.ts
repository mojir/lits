import type { NumberTheorySequenceReference } from '.'

export const perfectCubeReference: NumberTheorySequenceReference<'perfect-cube'> = {
  'TEMP-nth.perfect-cube-seq': {
    title: 'TEMP-nth.perfect-cube-seq',
    category: 'Number Theory',
    description: 'Generates the perfect cube numbers up to a specified length.',
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
      'let { perfect-cube-seq } = import("TEMP-nth");\nperfect-cube-seq(5)',
      'let { perfect-cube-seq } = import("TEMP-nth");\nperfect-cube-seq(20)',
    ],
  },
  'TEMP-nth.perfect-cube-take-while': {
    title: 'TEMP-nth.perfect-cube-take-while',
    category: 'Number Theory',
    description: 'Generates the perfect cube numbers while a condition is met.',
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
      'let { perfect-cube-take-while } = import("TEMP-nth");\nperfect-cube-take-while(-> $ <= 100)',
    ],
  },
  'TEMP-nth.perfect-cube-nth': {
    title: 'TEMP-nth.perfect-cube-nth',
    category: 'Number Theory',
    description: 'Generates the nth term of the perfect cube numbers.',
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
      'let { perfect-cube-nth } = import("TEMP-nth");\nperfect-cube-nth(1)',
      'let { perfect-cube-nth } = import("TEMP-nth");\nperfect-cube-nth(5)',
    ],
  },
  'TEMP-nth.perfect-cube?': {
    title: 'TEMP-nth.perfect-cube?',
    category: 'Number Theory',
    description: 'Checks if a number is in the perfect cube numbers.',
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
      'let { perfect-cube? } = import("TEMP-nth");\nperfect-cube?(7)',
      'let { perfect-cube? } = import("TEMP-nth");\nperfect-cube?(8)',
      'let { perfect-cube? } = import("TEMP-nth");\nperfect-cube?(9)',
    ],
  },
}
