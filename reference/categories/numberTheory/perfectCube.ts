import type { NumberTheorySequenceReference } from '.'

export const perfectCubeReference: NumberTheorySequenceReference<'perfect-cube'> = {
  'nth.perfect-cube-seq': {
    title: 'nth.perfect-cube-seq',
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
      'let nt = import("nth");\nnt.perfect-cube-seq(5)',
      'let nt = import("nth");\nnt.perfect-cube-seq(20)',
    ],
  },
  'nth.perfect-cube-take-while': {
    title: 'nth.perfect-cube-take-while',
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
      'let nt = import("nth");\nnt.perfect-cube-take-while(-> $ <= 100)',
    ],
  },
  'nth.perfect-cube-nth': {
    title: 'nth.perfect-cube-nth',
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
      'let nt = import("nth");\nnt.perfect-cube-nth(1)',
      'let nt = import("nth");\nnt.perfect-cube-nth(5)',
    ],
  },
  'nth.perfect-cube?': {
    title: 'nth.perfect-cube?',
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
      'let nt = import("nth");\nnt.perfect-cube?(7)',
      'let nt = import("nth");\nnt.perfect-cube?(8)',
      'let nt = import("nth");\nnt.perfect-cube?(9)',
    ],
  },
}
