import type { NumberTheorySequenceReference } from '.'

export const perfectCubeReference: NumberTheorySequenceReference<'perfect-cube'> = {
  'Number-Theory.perfect-cube-seq': {
    title: 'Number-Theory.perfect-cube-seq',
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
      'let { perfect-cube-seq } = import("Number-Theory");\nperfect-cube-seq(5)',
      'let { perfect-cube-seq } = import("Number-Theory");\nperfect-cube-seq(20)',
    ],
  },
  'Number-Theory.perfect-cube-take-while': {
    title: 'Number-Theory.perfect-cube-take-while',
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
      'let { perfect-cube-take-while } = import("Number-Theory");\nperfect-cube-take-while(-> $ <= 100)',
    ],
  },
  'Number-Theory.perfect-cube-nth': {
    title: 'Number-Theory.perfect-cube-nth',
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
      'let { perfect-cube-nth } = import("Number-Theory");\nperfect-cube-nth(1)',
      'let { perfect-cube-nth } = import("Number-Theory");\nperfect-cube-nth(5)',
    ],
  },
  'Number-Theory.perfect-cube?': {
    title: 'Number-Theory.perfect-cube?',
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
      'let { perfect-cube? } = import("Number-Theory");\nperfect-cube?(7)',
      'let { perfect-cube? } = import("Number-Theory");\nperfect-cube?(8)',
      'let { perfect-cube? } = import("Number-Theory");\nperfect-cube?(9)',
    ],
  },
}
